{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_x2048 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude


#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/allan/.cabal/bin"
libdir     = "/home/allan/.cabal/lib/x86_64-linux-ghc-9.2.8/x2048-0.1.0.0-inplace-x2048"
dynlibdir  = "/home/allan/.cabal/lib/x86_64-linux-ghc-9.2.8"
datadir    = "/home/allan/.cabal/share/x86_64-linux-ghc-9.2.8/x2048-0.1.0.0"
libexecdir = "/home/allan/.cabal/libexec/x86_64-linux-ghc-9.2.8/x2048-0.1.0.0"
sysconfdir = "/home/allan/.cabal/etc"

getBinDir     = catchIO (getEnv "x2048_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "x2048_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "x2048_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "x2048_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "x2048_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "x2048_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
