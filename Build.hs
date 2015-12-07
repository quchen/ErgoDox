{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}

-- | Fire-and-forget build script to configure the Infinity Ergodox
-- firmware.
--
-- Userland configuration is in "Config.hs".
module Build (main) where



import Data.Foldable
import Data.List (intercalate, isInfixOf)
import Data.Monoid
import System.Console.GetOpt
import System.Directory
import System.Exit

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



-- | The root directory for all output of this script.
buildPath :: FilePath
buildPath = ".build"

-- | Root directory for each Ergodox half.
buildPathFor :: Half -> FilePath
buildPathFor half = buildPath </> case half of
    L -> "ergodox-left"
    R -> "ergodox-right"

-- | Location of the file for each half.
firmwareFile :: Half -> FilePath
firmwareFile half = buildPathFor half </> "kiibohd.dfu.bin"



-- | Compile the firmware a .bin file that can be sent to the keyboard.
buildFirmware :: Half -> Rules ()
buildFirmware half = firmwareFile half %> (\_ -> do
    moveKlls half
    cmd (Traced "Creating firmware output dir")
        "mkdir -p" [buildPathFor half] // ()
    cmakeLists <- liftIO (do
        pwd <- makeAbsolute =<< getCurrentDirectory
        pure (pwd </> "controller") )
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
        [cmakeLists] // ()
    cmd (Cwd (buildPathFor half))
        (Traced ("Compiling " <> pprHalf half <> " half"))
        "make" )


-- | Move .kll files to their appropriate target folder so the compilation
-- script has them in the right locations.
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



-- | For some reason, the build system does not have the "kll" repo as a
-- submodule. This rule ensures an initial dummy build is done, during which
-- the "controller" repo sets itself up correctly.
initializeKllDir :: Rules ()
initializeKllDir = "controller/kll" %> \_ -> do
    let dummyPath = buildPath </> "dummy"
    cmd (Traced "Creating dummy output dir")
        "mkdir -p" [dummyPath] // ()
    cmd (Cwd dummyPath) (Traced "Preparing initial dummy build")
        "cmake ../.." // ()
    cmd (Cwd dummyPath) (Traced "Running dummy build")
        "make"



-- | Send generated .bin firmware to the keyboard. Requires it to be in
-- flashing state.
installFirmware :: Half -> Flash -> Action ()
installFirmware _ NoFlash = putNormal "Flashing skipped (enable with --flash)"
installFirmware half FlashAfterBuild = do
    need [firmwareFile half]
    let (wd, firmware) = splitFileName (firmwareFile half)
    (Exit e, Stderr stderr) <- cmd (Cwd wd) (Traced "Flashing")
        "sudo"
        ["-p", "Root privileges needed to flash uC. Password: "]
        "--"
        "dfu-util"
        ["--download", firmware]
    case e of
        ExitSuccess -> pure ()
        ExitFailure _e | "No DFU capable USB device available" `isInfixOf` stderr
            -> fail "No keyboard in flash mode found"
        _else -> fail "dlflfdj"



-- | Remove all build files by this script and the wrapped "controller" build
-- system.
clean :: Rules ()
clean = phony "clean" (do
    removeFilesAfter ".build" ["//*"]
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



data Flags = FlashFlag -- ^ Send firmware to the keyboard after building?
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
