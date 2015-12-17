-- | Fire-and-forget build script to configure the Infinity Ergodox
-- firmware.
module Main (main) where



import Data.Foldable
import Data.List (intercalate, isInfixOf)
import Data.Monoid
import System.Console.GetOpt
import System.Directory
import System.Exit

import Development.Shake hiding (addPath)
import Development.Shake.FilePath

import Build.Oracles
import Build.Types
import qualified Build.WrappedBuildSystemConfig as Build



ppr :: PrimaryHalf -> String
ppr = \case
    L -> "left"
    R -> "right"

leftHalf :: Flash -> Rules ()
leftHalf = phonyForHalf L

rightHalf :: Flash -> Rules ()
rightHalf = phonyForHalf R

phonyForHalf :: PrimaryHalf -> Flash -> Rules ()
phonyForHalf primaryHalf flash = phony (ppr primaryHalf) (do
    need [firmwareFile primaryHalf]
    installFirmware )
  where

    -- Send generated .bin firmware to the keyboard. Requires it to be in
    -- flashing state.
    installFirmware :: Action ()
    installFirmware = case flash of
        NoFlash -> putNormal "Flashing skipped (enable with --flash)"
        FlashAfterBuild -> do
            need [firmwareFile primaryHalf]
            let (wd, firmware) = splitFileName (firmwareFile primaryHalf)
            (Exit e, Stderr stderr) <- cmd (Cwd wd) (Traced "Flashing")
                "sudo"
                ["-p", "Root privileges needed to flash uC. Password: "]
                "--"
                "dfu-util"
                ["--download", firmware]
            case e of
                ExitSuccess -> pure ()
                ExitFailure _e | "No DFU capable USB device" `isInfixOf` stderr
                    -> fail "No keyboard in flash mode found."
                _else -> fail stderr



-- | Shake build files output directory
buildPath :: FilePath
buildPath = ".build"

-- | Final location of the firmware
firmwareFile :: PrimaryHalf -> FilePath
firmwareFile primaryHalf =
    buildPath </> "ergodox-" <> ppr primaryHalf <.> "dfu.bin"



-- | Compile the firmware a .bin file that can be sent to the keyboard
buildFirmware :: PrimaryHalf -> Rules ()
buildFirmware primaryHalf = firmwareFile primaryHalf %> (\out -> do
    moveKlls
    cmake
    make
    extractFirmwareTo out )
  where

    -- Move .kll files to their appropriate target folder so the compilation
    -- script has them in the right locations
    moveKlls :: Action ()
    moveKlls = do
        BaseMap baseMap <- baseMapConfig primaryHalf
        for_ baseMap (moveKll "Layout" "controller/Scan/MDErgo1")
        need ["controller/kll"]
        DefaultMap defaultMap <- defaultMapConfig
        for_ defaultMap (moveKll "Layout" "controller/kll/layouts")
        PartialMaps layers <- partialMapsConfig
        for_ layers (\(Layer layer) ->
            for_ layer (moveKll "Layout" "controller/kll/layouts") )
      where
        moveKll srcDir destDir = \kll ->
            let kllFile = kll -<.> "kll"
                src = srcDir </> kllFile
                dest = destDir </> takeFileName kllFile
            in copyFileChanged src dest



    cmake :: Action ()
    cmake = do
        liftIO (createDirectoryIfMissing True wrappedBuildPath)
        baseMap <- do
            BaseMap baseMapKlls <- baseMapConfig primaryHalf
            pure (unwords baseMapKlls)
        defaultMap <- do
            DefaultMap defaultMapKlls <- defaultMapConfig
            pure (unwords defaultMapKlls)
        partialMaps <- do
            PartialMaps layers <- partialMapsConfig
            let collapsedLayers = [ unwords layer | Layer layer <- layers ]
                collapsedPartialMaps = intercalate ";" collapsedLayers
            pure collapsedPartialMaps
        cmd (Cwd wrappedBuildPath)
            (Traced ("Generating " <> ppr primaryHalf <> " makefile"))
            "cmake"
            [ "-DCHIP="         <> chip
            , "-DCOMPILER="     <> compiler
            , "-DScanModule="   <> scanModule
            , "-DMacroModule="  <> macroModule
            , "-DOutputModule=" <> outputModule
            , "-DDebugModule="  <> debugModule
            , "-DBaseMap="      <> baseMap
            , "-DDefaultMap="   <> defaultMap
            , "-DPartialMaps="  <> partialMaps ]
            "../.."
      where
        Chip         chip         = Build.chip
        Compiler     compiler     = Build.compiler
        ScanModule   scanModule   = Build.scanModule
        MacroModule  macroModule  = Build.macroModule
        OutputModule outputModule = Build.outputModule
        DebugModule  debugModule  = Build.debugModule



    make :: Action ()
    make = cmd
        (Cwd wrappedBuildPath)
        (Traced ("Compiling " <> ppr primaryHalf <> " primaryHalf"))
        "make -j"

    extractFirmwareTo :: FilePath -> Action ()
    extractFirmwareTo out = copyFileChanged wrappedFirmwareFile out

    -- | Directory the wrapped build system puts its files to
    wrappedBuildPath :: FilePath
    wrappedBuildPath = "controller/build/ergodox-" <> ppr primaryHalf

    -- | Compiled firmware file as generated by the inner firmware
    wrappedFirmwareFile :: FilePath
    wrappedFirmwareFile = wrappedBuildPath </> "kiibohd.dfu.bin"



-- | For some reason, the build system does not have the "kll" repo as a
-- submodule. This rule ensures an initial dummy build is done, during which
-- the "controller" repo sets itself up correctly.
initializeKllDir :: Rules ()
initializeKllDir = "controller/kll" %> \_ -> do
    let dummyPath = "controller/dummy"
    liftIO (createDirectoryIfMissing True dummyPath)
    cmd (Cwd dummyPath) (Traced "Preparing initial dummy build")
        "cmake .."



-- | Remove all build files by this script and the wrapped "controller" build
-- system.
clean :: Rules ()
clean = phony "clean" (do
    putNormal ("Deleting own build folder: " <> buildPath)
    removeFilesAfter buildPath ["//*"]
    putNormal "Deleting cabal build folder"
    removeFilesAfter "dist" ["//*"]
    putNormal "Cleaning wrapped build systems in controller/"
    gitClean "controller"
    gitClean "controller/kll" )
  where
    gitClean :: FilePath -> Action ()
    gitClean dir = cmd (Cwd dir) (Traced "") "git clean -dfq"



-- | Postfix version of 'unit'
-- (//) :: m () -> a -> m ()
-- x // _ = unit x
-- infix 1 //



-- | Should the keyboard be automatically flashed after building finishes?
data Flash = FlashAfterBuild | NoFlash

data Flags = FlashFlag -- ^ Send firmware to the keyboard after building?
    deriving Eq

flagSpecs :: [OptDescr (Either a Flags)]
flagSpecs =
    [Option "" ["flash"] (NoArg (Right FlashFlag)) "Flash keyboard"]



main :: IO ()
main = shakeArgsWith options flagSpecs (\flags targets -> return (Just (
    let rules = ruleRecipes (flashFlag flags)
        runRules = rules `runGiven` targets
    in runRules )))
  where

    ruleRecipes flash = mconcat [halves flash, firmware, clean, aux, oracles]
      where
        halves   = leftHalf <> rightHalf
        firmware = buildFirmware L <> buildFirmware R
        aux      = initializeKllDir

    rules `runGiven` [] = rules
    rules `runGiven` targets = want targets >> withoutActions rules

    flashFlag flags | FlashFlag `elem` flags = FlashAfterBuild
                    | otherwise              = NoFlash

    options = shakeOptions
