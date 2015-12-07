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
    need [firmwareFile half]
    installFirmware half flash
    )



firmwareFile :: Half -> FilePath
firmwareFile half = buildPathFor half </> "kiibohd.dfu.bin"

buildPath :: FilePath
buildPath = "controller/build"

buildPathFor :: Half -> FilePath
buildPathFor half = buildPath </> case half of
    L -> "ergodox-left"
    R -> "ergodox-right"



-- | Compile the firmware into an executable. Running it while a primed
-- keyboard is connected will then flash it.
buildFirmware :: Half -> Rules ()
buildFirmware half = firmwareFile half %> (\_ -> do
    moveKlls half
    cmd (Traced "Creating firmware output dir")
        "mkdir -p" [buildPathFor half] // ()
    cmd (Cwd (buildPathFor half))
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
    cmd (Cwd (buildPathFor half))
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

initializeKllDir :: Rules ()
initializeKllDir = "controller/kll" %> \_ -> do
    let dummyPath = buildPath </> "dummy"
    cmd (Traced "Creating dummy output dir")
        "mkdir -p" [dummyPath] // ()
    cmd (Cwd dummyPath) (Traced "Preparing initial dummy build")
        "cmake ../.." // ()
    cmd (Cwd dummyPath) (Traced "Running dummy build")
        "make" // ()



installFirmware :: Half -> Flash -> Action ()
installFirmware _ NoFlash = putNormal "Flashing skipped (enable with --flash)"
installFirmware half FlashAfterBuild = do
    need [firmwareFile half]
    primeController
    let (wd, firmware) = splitFileName (firmwareFile half)
    cmd (Cwd wd) (Traced "Flashing")
        "sudo dfu-util"
        ["--download", firmware]

primeController :: Action ()
primeController = do
    ensureSudo
    tty <- firstTty
    cmd (Traced "Priming keyboard")
        "sudo bash -c"
        ["printf \"reload\r\" > " <> tty] // ()
    let waitSeconds = "2"
    cmd (Traced ("Waiting " <> waitSeconds <> " for microcontroller"))
        "sleep" [waitSeconds]

ensureSudo :: Action ()
ensureSudo = getEnv "EUID" >>= \case
    Just "0"   -> pure ()
    Just _not0 -> fail "Must be root to run with --flash"
    Nothing    -> fail "EUID not set"

firstTty :: Action String
firstTty = do
    let pat = "ttyACM*"
    (Exit _, Stdout ttys, Stderr ()) <- cmd "find /dev" ["-name", pat]
    case lines ttys of
        []    -> fail ("No suitable tty found for pattern " <> pat)
        [tty] -> pure tty
        _else -> fail ("More than one " <> pat <> " found:\n" <> ttys)



clean :: Rules ()
clean = phony "clean" (do
    gitClean "controller"
    gitClean "controller/kll" )
  where
    gitClean :: FilePath -> Action ()
    gitClean dir =
        cmd (Cwd dir) (Traced ("git clean " <> dir))
            "git clean -df"



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
    [Option "" ["flash"] (NoArg (Right FlashFlag)) "Flash keyboard"]



main :: IO ()
main = shakeArgsWith options flagSpecs (\flags targets -> return (Just (
    let flashFlag | FlashFlag `elem` flags = FlashAfterBuild
                  | otherwise              = NoFlash
        rules' = rules flashFlag
        rules'' | null targets = rules'
                | otherwise = want targets >> withoutActions rules'
    in rules'' )))
  where
    rules flash = mconcat [halves flash, firmware, clean, aux]
    halves      = leftHalf <> rightHalf
    firmware    = buildFirmware L <> buildFirmware R
    aux         = initializeKllDir

    options = shakeOptions
        { shakeStaunch = True -- Build as much as possible
        , shakeThreads = 0    -- 0 = num cpus
        }
