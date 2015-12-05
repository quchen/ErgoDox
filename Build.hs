{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}

module Build (main) where



import Development.Shake hiding (addPath)
import Development.Shake.FilePath
import Data.Monoid
import Data.Foldable



data Half = L | R

leftHalf, rightHalf :: Rules ()
leftHalf = phony "left" (buildHalf L)
rightHalf = phony "right" (buildHalf R)



loaderElf :: Half -> FilePath
loaderElf = \case
    L -> "controller/Keyboards/ergodox-left/load"
    R -> "controller/Keyboards/ergodox-right/load"



buildHalf :: Half -> Action ()
buildHalf half = do
    ensureSudo
    need [loaderElf half]
    installFirmware half



buildFirmware :: Rules ()
buildFirmware = loadFirmwareElfs &%> \_ -> do
    need ["controller/kll", "controller/Keyboards/q-dox.bash"]
    baseKlls <- getDirectoryFiles "" ["controller/Scan/MDErgo1/q-*.kll"]
    customKlls <- getDirectoryFiles "" ["controller/kll/layouts/q-*.kll"]
    need (baseKlls <> customKlls)
    cmd (Cwd "controller/Keyboards") env "./q-dox.bash"
  where
    loadFirmwareElfs = map loaderElf [L,R]
    env = [ AddEnv "ScanModule"  "MDErgo1"
          , AddEnv "MacroModule" "PartialMap"
          , AddEnv "OutputModule""pjrcUSB"
          , AddEnv "DebugModule" "full"
          , AddEnv "Chip"        "mk20dx256vlh7"
          , AddEnv "Compiler"    "gcc" ]



kllDir :: Rules ()
kllDir = "controller/kll" %> \_ ->
    cmd (Cwd "controller/Keyboards") (Traced "Dummy build for initialization")
        "./template.bash"



qdox :: Rules ()
qdox = "controller/Keyboards/q-dox.bash" %> \_ -> do
    klls <- getDirectoryFiles "" ["klls/*.kll"]
    for_ klls (\kll -> do
        copyFileChanged kll ("controller/Scan/MDErgo1" </> takeFileName kll)
        copyFileChanged kll ("controller/kll/layouts" </> takeFileName kll) )
    copyFileChanged "klls/q-dox.bash" "controller/Keyboards/q-dox.bash"



ttyecho :: Rules ()
ttyecho = "ttyecho/ttyecho" %> \out ->
    cmd (Cwd (takeDirectory out)) "make ttyecho"



ensureSudo :: Action ()
ensureSudo = getEnv "EUID" >>= \case
    -- Nothing                 -> fail "EUID not set"
    -- Just euid | euid /= "0" -> fail "Script must be run as root"
    _else                   -> pure ()



installFirmware :: Half -> Action ()
installFirmware half = do
    need [loaderElf half]
    -- primeController
    let (wd, elf) = splitFileName (loaderElf half)
    cmd (Cwd wd) ("sudo ./" <> elf)



-- primeController :: Action ()
-- primeController = do
--     need ["ttyecho/ttyecho"]
--     tty <- firstTty
--     cmd (Cwd "ttyecho") "sudo -- ./ttyecho -n" [tty] "reload"



-- firstTty :: Action String
-- firstTty = do
--     let pat = "ttyACM*"
--     Stdout ttys <- cmd "find /dev" ["-name", pat]
--     case lines ttys of
--         []    -> fail ("No tty found with " <> pat)
--         [tty] -> pure tty
--         _else -> fail ("More than one " <> pat <> " found:\n" <> ttys)



clean :: Rules ()
clean = phony "clean" (do
    cmd (Cwd "controller") (Traced "cleaning controller/")
        "git clean -df" // ()
    cmd (Cwd "controller/kll") (Traced "cleaning controller/kll/")
        "git clean -df" )



-- | Postfix version of 'unit'
(//) :: m () -> a -> m ()
x // _ = unit x
infix 1 //



main :: IO ()
main = shakeArgs options rules
  where
    rules = mconcat [leftHalf, rightHalf, ttyecho, buildFirmware, qdox, clean, kllDir]
    options = shakeOptions
