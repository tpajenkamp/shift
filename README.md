# *shift*
Sokoban-like game in Haskell.


## Goal

Move every crate onto an empty target. You cannot pull objects,
only shift them.


## Building *shift*

Required: *ghc*, *cabal*, *gtk3*

$ cabal sandbox init

$ cabal install gtk2hs-buildtools

$ cabal install --only-dependencies

$ cabal build

$ cabal install


## Running *shift*

$ shift [flags] [level file]

Default level location is "levels/level.txt". If the stated level does not
exist a level selection dialog is shown.

Flag       | Effect
-----------|------------
-graphical | (default) playing field based on graphical bitmaps 
-ascii     | playing field drawn in ASCII characters


## Controls

Key        | Effect
-----------|------------
W,A,S,D    | **move** character
arrow keys | **move** character
R          | **restart** current level
O          | **open** level file selection prompt
N          | go to **next** level in level sequence
P          | go to **previous** level in level sequence
-, +       | **undo** and **redo** a step

