ErgoDox build system
====================

Fire-and-forget build system for configuring the Infinity Ergodox.



Usage
-----

1. Clone this repo
2. Install current GHC+Cabal for running Haskell
3. Install prerequisite packages
   ```
   sudo apt-get install cmake ctags libusb-1.0-0-dev
   sudo apt-get install binutils-arm-none-eabi gcc-arm-none-eabi dfu-util
   ```
4. Edit layouts in `klls/` and configure `Config.hs`
5. Run `./build (left|right)` (first time installs stuff so it takes longer)
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
