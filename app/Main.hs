{- implementation of 2048 using the store comonad -}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module Main where
import           Data.MemoCombinators as Memo
import           System.IO            (BufferMode (..), hSetBuffering, hSetEcho, stdin)
import           System.Random        (randomRIO)

class Functor w => Comonad w where
  extract   :: w a -> a
  duplicate :: w a -> w (w a)
  duplicate = extend id
  extend :: (w a -> b) -> w a -> w b
  extend f = fmap f . duplicate

data Store s a = Store (s -> a) s

instance Functor (Store s) where
  fmap f (Store g s) = Store (f . g) s

instance Comonad (Store s) where
  extract   (Store f s) = f s
  duplicate (Store f s) = Store (Store f) s

data Direction  = L | R | U | D deriving (Eq)
data TileStatus = Normal | NewTile | Merged
type Position   = (Int, Int)
type Value      = Int
type Grid a     = Store Position a

max_size :: Int
max_size = 3

memo2d :: Memo Position
memo2d = Memo.pair Memo.integral Memo.integral

tab :: Memo s -> Store s a -> Store s a
tab f (Store g s) = Store (f g) s

experiment :: Functor f => (s -> f s) -> Store s a -> f a
experiment f (Store g s) = fmap g (f s)

row_indices :: Direction -> Position -> [Position]
row_indices dir (x, y) =
  case dir of
    L -> [(c, y) | c <- [0..max_size]]
    R -> [(c, y) | c <- [max_size, max_size - 1..0]]
    D -> [(x, r) | r <- [0..max_size]]
    U -> [(x, r) | r <- [max_size, max_size - 1..0]]

to_row :: Direction -> Grid a -> [a]
to_row dir = experiment (row_indices dir)

move_row :: [Value] -> [Value]
move_row xs = xs' ++ replicate (length xs - length xs') 0
  where xs'              = move_row' (filter (/= 0) xs)
        move_row' []     = []
        move_row' [x]    = [x]
        move_row' (x:y:rest)
          | x == y       = x + y : move_row' rest
          | otherwise    = x : move_row' (y : rest)

move_row_merged :: [Value] -> [Bool]
move_row_merged xs = merged ++ replicate (length xs - length merged) False
  where merged         = go (filter (/= 0) xs)
        go []          = []
        go [_]         = [False]
        go (x:y:rest)
          | x == y     = True : go rest
          | otherwise  = False : go (y : rest)

game_rule :: Direction -> Grid Value -> Value
game_rule dir (Store f s) =
  case dir of
    L ->         move_row (to_row dir g) !! x
    R -> reverse (move_row (to_row dir g)) !! x
    D ->         move_row (to_row dir g) !! y
    U -> reverse (move_row (to_row dir g)) !! y
  where (x, y) = s
        g       = Store f s

game_rule_merged :: Direction -> Grid Value -> Bool
game_rule_merged dir (Store f s) =
  case dir of
    L ->         move_row_merged (to_row dir g) !! x
    R -> reverse (move_row_merged (to_row dir g)) !! x
    D ->         move_row_merged (to_row dir g) !! y
    U -> reverse (move_row_merged (to_row dir g)) !! y
  where (x, y) = s
        g       = Store f s

all_positions :: [Position]
all_positions = [(x, y) | x <- [0..max_size], y <- [0..max_size]]

apply_move :: Direction -> Grid Value -> (Grid Value, Position -> TileStatus)
apply_move dir g = (new_g, status)
  where new_g              = extend (game_rule dir . tab memo2d) g
        Store merged_f _   = tab memo2d $ extend (game_rule_merged dir . tab memo2d) g
        status p           = if merged_f p then Merged else Normal

grids_equal :: Grid Value -> Grid Value -> Bool
grids_equal (Store f1 _) (Store f2 _) = all (\p -> f1 p == f2 p) all_positions

empty_tiles :: Grid Value -> [Position]
empty_tiles (Store f _) = filter (\p -> f p == 0) all_positions

add_new_tile :: Grid Value -> (Position -> TileStatus) -> IO (Grid Value, Position -> TileStatus)
add_new_tile g@(Store f s) prev_status
  | null t    = return (g, prev_status)
  | otherwise = do
      i   <- randomRIO (0, length t - 1)
      v   <- (\r -> if (r :: Int) < 9 then 2 else 4) <$> randomRIO (0, 9)
      let pos = t !! i
      return (Store (\p -> if p == pos then v else f p) s,
              \p -> if p == pos then NewTile else prev_status p)
  where t = empty_tiles g

is_won :: Grid Value -> Bool
is_won (Store f _) = 2048 `elem` map f all_positions

is_lost :: Grid Value -> Bool
is_lost g = null (empty_tiles g) && all (\d -> grids_equal g (apply_move_only d g)) [L, R, U, D]
  where apply_move_only d = fst . apply_move d

initial_state :: IO (Grid Value, Position -> TileStatus)
initial_state = do
  (g1, s1) <- add_new_tile (Store (const 0) (0, 0)) (const Normal)
  add_new_tile g1 s1

end_condition :: Grid Value -> IO Bool
end_condition g
  | is_won g  = putStrLn "You won!"  >> return True
  | is_lost g = putStrLn "You lost!" >> return True
  | otherwise = return False

colorize :: TileStatus -> String -> String
colorize Normal  s = s
colorize NewTile s = "\ESC[31m" ++ s ++ "\ESC[0m"
colorize Merged  s = "\ESC[32m" ++ s ++ "\ESC[0m"

render :: Grid Value -> (Position -> TileStatus) -> String
render (Store f _) status = unlines $ map (unwords . map pad_cell) rows
  where rows     = reverse [[(x, y) | x <- [0..max_size]] | y <- [0..max_size]]
        pad_cell pos =
          let v      = f pos
              padded = replicate (4 - length (show v)) ' ' ++ show v
          in colorize (if v == 0 then Normal else status pos) padded

get_direction :: IO (Maybe Direction)
get_direction = getChar >>= \c -> case c of
  'w'    -> return (Just U)
  'a'    -> return (Just L)
  's'    -> return (Just D)
  'd'    -> return (Just R)
  '\ESC' -> getChar >>= \c2 -> case c2 of
    '[' -> getChar >>= \c3 -> case c3 of
      'A' -> return (Just U)
      'B' -> return (Just D)
      'C' -> return (Just R)
      'D' -> return (Just L)
      _   -> return Nothing
    _   -> return Nothing
  _      -> return Nothing

main_loop :: (Grid Value, Position -> TileStatus) -> IO ()
main_loop (g, status) = do
  putStr "\ESC[2J\ESC[H"
  putStrLn "2048 -- move with wasd or arrow keys, reach 2048 to win"
  putStrLn (render g status)
  end <- end_condition g
  if end then return () else do
    input <- get_direction
    case input of
      Just d  -> do
        let (g', status') = apply_move d g
        add_new_tile g' status' >>= main_loop
      Nothing -> main_loop (g, status)

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  initial_state >>= main_loop
