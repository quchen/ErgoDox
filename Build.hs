{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}

module Build (main) where



import Development.Shake hiding (addPath)
import Development.Shake.FilePath
import Data.Monoid
import Data.List (intercalate)
import Data.Foldable



import Config ( BaseMap(..), Chip(..), Compiler(..), DebugModule(..)
              , DefaultMap(..), Layer(..), MacroModule(..), OutputModule(..)
              , PartialMaps(..), ScanModule(..) )
import qualified Config as Config



data Half = L | R

pprHalf :: Half -> String
pprHalf = \case
    L -> "left"
    R -> "right"

leftHalf, rightHalf :: Rules ()
[leftHalf, rightHalf] = map makePhony [L,R]
  where
    makePhony half = phony (pprHalf half) (do
        need [loaderElf half]
        -- installFirmware half
        )

loaderElf :: Half -> FilePath
loaderElf half = "controller" </> "Keyboards" </> buildPath half </> "load"

buildPath :: Half -> FilePath
buildPath = \case
    L -> "ergodox-left"
    R -> "ergodox-right"


buildLoader :: Half -> Rules ()
buildLoader half = loaderElf half %> \_ -> do
    moveKlls

    cmd (Cwd "controller/Keyboards")
        (Traced "Creating build output dir")
        "mkdir -p" [buildPath half] // ()
    cmd (Cwd ("controller/Keyboards" </> buildPath half))
        (Traced ("Building " <> pprHalf half <> " half"))
        "cmake"
        ("-DCHIP="         <> let Chip         x = Config.chip         in x)
        ("-DCOMPILER="     <> let Compiler     x = Config.compiler     in x)
        ("-DScanModule="   <> let ScanModule   x = Config.scanModule   in x)
        ("-DMacroModule="  <> let MacroModule  x = Config.macroModule  in x)
        ("-DOutputModule=" <> let OutputModule x = Config.outputModule in x)
        ("-DDebugModule="  <> let DebugModule  x = Config.debugModule  in x)
        ("-DBaseMap="      <> let BaseMap x = Config.baseMap
                              in intercalate " " x )
        ("-DDefaultMap="   <> let DefaultMap x = Config.defaultMap
                              in intercalate " " x )
        ("-DPartialMaps="  <> let PartialMaps partialMaps = Config.partialMaps
                                  layers = [ intercalate " " layer | Layer layer <- partialMaps]
                              in intercalate ";" layers )
        "../.."

  where

    -- Move .kll files to their appropriate target folder so the compilation
    -- script has them in the right locations
    moveKlls = do
        let BaseMap baseMap = Config.baseMap
        for_ baseMap (\kll ->
            let kllFile = kll <.> "kll"
                src = "klls" </> kllFile
                dest = "controller/Scan/MDErgo1" </> kllFile
            in copyFileChanged src dest )

        let DefaultMap defaultMap = Config.defaultMap
        need ["controller/kll"]
        for_ defaultMap (\kll ->
            let kllFile = kll <.> "kll"
                src = "klls" </> kllFile
                dest = "controller/kll/layouts" </> kllFile
            in copyFileChanged src dest )

        let PartialMaps layers = Config.partialMaps
        need ["controller/kll"]
        for_ layers (\(Layer layer) ->
            for_ layer (\kll ->
                let kllFile = kll <.> "kll"
                    src = "klls" </> kllFile
                    dest = "controller/kll/layouts" </> kllFile
                in copyFileChanged src dest ))



kllDir :: Rules ()
kllDir = "controller/kll" %> \_ ->
    cmd (Cwd "controller/Keyboards")
        (Traced "Dummy build for initialization of kll subdir")
        "./template.bash"



ttyecho :: Rules ()
ttyecho = "ttyecho/ttyecho" %> \out ->
    cmd (Cwd (takeDirectory out)) "make ttyecho"



-- ensureSudo :: Action ()
-- ensureSudo = getEnv "EUID" >>= \case
--     -- Nothing                 -> fail "EUID not set"
--     -- Just euid | euid /= "0" -> fail "Script must be run as root"
--     _else                   -> pure ()



-- installFirmware :: Half -> Action ()
-- installFirmware half = do
--     need [loaderElf half]
--     -- primeController
--     let (wd, elf) = splitFileName (loaderElf half)
--     cmd (Cwd wd) ("sudo ./" <> elf)



-- primeController :: Action ()
-- primeController = do
--     need ["ttyecho/ttyecho"]
--     ensureSudo
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
    rules = mconcat [leftHalf, rightHalf, ttyecho, buildLoader L, buildLoader R, clean, kllDir]
    options = shakeOptions
