ErgoDox build system
====================

Fire-and-forget build system for configuring the Infinity Ergodox.



Usage
-----

1. Clone this repo
2. Install current GHC+Cabal for running Haskell
3. Edit layouts in `klls/` and configure `Config.hs`
4. Run `./build (left|right)` (first time installs stuff so it takes longer)
    - Build with `--flash` to flash the keyboard after building

Directory contents
------------------

- `klls`: Key layout configuration
- `controller`: Firmware compilation
- `ttyecho`: Write to other ttys, useful for automatic flashing
- `build`: The user-facing script
- `Build.hs`: The actual build system
- `Config.hs`: Configuration of keyboard layers
- `README.hs`: This file

To do
-----

- Auto-prime the microcontroller for flashing
