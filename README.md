# Sudoku

This is nothing but a playground for trying to get a hold on haskell.
It reads a sudoku game from stdin and outputs the solution to stdout.

## Usage

    ghc --make sudoku.hs
    ./sudoku < puzzle.txt

## Todo

* lists seem to be a bad idea if you want random access
* brute forcing combined with inappropriate datatypes slows down even compiled languages (surprise!)
