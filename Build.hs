{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}

-- | Fire-and-forget build script to configure the Infinity Ergodox
-- firmware.
--
-- Userland configuration is in "Config.hs".
module Build (main) where



import Data.Foldable
import Data.List (intercalate)
import Data.Monoid
import System.Console.GetOpt

import Development.Shake hiding (addPath)
import Development.Shake.FilePath

import Config ( BaseMap(..), Chip(..), Compiler(..), DebugModule(..)
              , DefaultMap(..), Layer(..), MacroModule(..), OutputModule(..)
              , PartialMaps(..), ScanModule(..)
              , Half(..) )
import qualified Config as Cfg



pprHalf :: Half -> String
pprHalf = \case
    L -> "left"
    R -> "right"



leftHalf :: Flash -> Rules ()
leftHalf = phonyForHalf L

rightHalf :: Flash -> Rules ()
rightHalf = phonyForHalf R

phonyForHalf :: Half -> Flash -> Rules ()
phonyForHalf half flash = phony (pprHalf half) (do
    need [loaderElf half]
    installFirmware half flash
    )



-- | Path to the loader executable. Running this will flash the keyboard.
loaderElf :: Half -> FilePath
loaderElf half = buildPath half </> "load"

buildPath :: Half -> FilePath
buildPath half = "controller/build" </> case half of
    L -> "ergodox-left"
    R -> "ergodox-right"



-- | Compile the firmware into an executable. Running it while a primed
-- keyboard is connected will then flash it.
buildLoader :: Half -> Rules ()
buildLoader half = loaderElf half %> (\_ -> do
    moveKlls half
    cmd (Traced "Creating build output dir")
        "mkdir -p" [buildPath half] // ()
    cmd (Cwd (buildPath half))
        (Traced ("Generating " <> pprHalf half <> " makefile"))
        "cmake"
        ["-DCHIP="         <> let Chip         x = Cfg.chip         in x]
        ["-DCOMPILER="     <> let Compiler     x = Cfg.compiler     in x]
        ["-DScanModule="   <> let ScanModule   x = Cfg.scanModule   in x]
        ["-DMacroModule="  <> let MacroModule  x = Cfg.macroModule  in x]
        ["-DOutputModule=" <> let OutputModule x = Cfg.outputModule in x]
        ["-DDebugModule="  <> let DebugModule  x = Cfg.debugModule  in x]
        ["-DBaseMap="      <> let BaseMap x = Cfg.baseMap half
                              in intercalate " " x ]
        ["-DDefaultMap="   <> let DefaultMap x = Cfg.defaultMap
                              in intercalate " " x ]
        ["-DPartialMaps="  <> let PartialMaps pms = Cfg.partialMaps
                                  layers = [ intercalate " " layer
                                           | Layer layer <- pms]
                              in intercalate ";" layers ]
        "../.." // ()
    cmd (Cwd (buildPath half))
        (Traced ("Compiling " <> pprHalf half <> " half"))
        "make" )


-- | Move .kll files to their appropriate target folder so the compilation
-- script has them in the right locations
moveKlls :: Half -> Action ()
moveKlls half = sequence_ [moveBaseMap, moveDefaultMap, moveLayers]
  where
    moveBaseMap = do
        let BaseMap baseMap = Cfg.baseMap half
        for_ baseMap (\kll ->
            let kllFile = kll <.> "kll"
                src = "klls" </> kllFile
                dest = "controller/Scan/MDErgo1" </> kllFile
            in copyFileChanged src dest )
    moveDefaultMap = do
        let DefaultMap defaultMap = Cfg.defaultMap
        need ["controller/kll"]
        for_ defaultMap (\kll ->
            let kllFile = kll <.> "kll"
                src = "klls" </> kllFile
                dest = "controller/kll/layouts" </> kllFile
            in copyFileChanged src dest )
    moveLayers = do
        let PartialMaps layers = Cfg.partialMaps
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



installFirmware :: Half -> Flash -> Action ()
installFirmware _ NoFlash = putNormal "Flashing skipped (enable with --flash)"
installFirmware half FlashAfterBuild = do
    need [loaderElf half]
    -- primeController
    let (wd, elf) = splitFileName (loaderElf half)
    cmd (Cwd wd) (Traced "Flashing")
        ("sudo ./" <> elf)



-- primeController :: Action ()
-- primeController = do
--     need ["ttyecho/ttyecho"]
--     ensureSudo
--     tty <- firstTty
--     cmd (Cwd "ttyecho") "sudo -- ./ttyecho -n" [tty] "reload"

-- ttyecho :: Rules ()
-- ttyecho = "ttyecho/ttyecho" %> \out ->
--     cmd (Cwd (takeDirectory out)) "make ttyecho"

-- ensureSudo :: Action ()
-- ensureSudo = getEnv "EUID" >>= \case
--     -- Nothing                 -> fail "EUID not set"
--     -- Just euid | euid /= "0" -> fail "Script must be run as root"
--     _else                   -> pure ()

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
    cmd (Cwd "controller") (Traced "git clean controller")
        "git clean -df" // ()
    cmd (Cwd "controller/kll") (Traced "git clean controller/kll")
        "git clean -df" // ()
    )



-- | Postfix version of 'unit'
(//) :: m () -> a -> m ()
x // _ = unit x
infix 1 //



-- | Should the keyboard be automatically flashed after building finishes?
data Flash = FlashAfterBuild | NoFlash



data Flags = FlashFlag
    deriving Eq

flagSpecs :: [OptDescr (Either a Flags)]
flagSpecs =
    [Option "" ["flash"] (NoArg (Right FlashFlag)) "Flash after compilation"]



main :: IO ()
main = shakeArgsWith options flagSpecs (\flags targets -> return (Just (
    let flashFlag | FlashFlag `elem` flags = FlashAfterBuild
                  | otherwise              = NoFlash
        rules' = rules flashFlag
        rules'' | null targets = rules'
                | otherwise = want targets >> withoutActions rules'
    in rules'' )))
  where
    rules flash = mconcat [leftHalf flash, rightHalf flash, buildLoader L, buildLoader R, clean, kllDir]
    options = shakeOptions
        { shakeStaunch = True -- Build as much as possible
        , shakeThreads = 0    -- 0 = num cpus
        }
