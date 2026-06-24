# Comonadic 2048

A terminal implementation of 2048 using the store comonad in Haskell. See the accompanying [blog post](https://www.melkyway.ca/posts/2023-08-17-comonadic2048.html) for an explanation of the approach.

## Usage

Run the game with:

```
nix run
```

Move tiles with `wasd` or the arrow keys. Reach 2048 to win.

Newly spawned tiles are shown in red, and tiles that just merged are shown in green.

## Development

This project uses Nix flakes. Enter the dev shell with:

```
direnv allow
```

Then build or run with cabal:

```
cabal build
cabal run
```
