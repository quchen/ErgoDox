ErgoDox build system
====================

Fire-and-forget build system for configuring the Infinity Ergodox.



Usage
-----

### Installation

1. Clone this repo
2. Install current GHC+Cabal for running Haskell
3. Install prerequisite packages

  ```
  sudo apt-get install       \
      cmake                  \
      ctags                  \
      libusb-1.0-0-dev       \
      binutils-arm-none-eabi \
      gcc-arm-none-eabi      \
      dfu-util
  ```

4. Run `./build` to set up the build system

### Execution

1. Edit layouts in `Layout/` and configure their interplay in
   `Layout/Config.hs`
2. Run `./build (left|right)`
    - Build with `--flash` to flash the keyboard after building



Directory contents
------------------

- `controller`: Firmware compilation
- `Build`: Build system files
- `Layout`: Key layout configuration
- `build`: Build system entry point
- `README.md`: This file
