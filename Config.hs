module Config (
    -- * Custom configuration
    baseMap,
    defaultMap,
    partialMaps,

    -- * Should not change in general
    scanModule,
    macroModule,
    outputModule,
    debugModule,
    chip,
    compiler,

    -- * Type safety newtypes
    BaseMap(..),
    Chip(..),
    Compiler(..),
    DebugModule(..),
    DefaultMap(..),
    Layer(..),
    MacroModule(..),
    OutputModule(..),
    PartialMaps(..),
    ScanModule(..)
) where



baseMap :: BaseMap
baseMap = BaseMap
    [ "base-left"
    , "flash-remote"
    , "flash"
    , "switch-to-slave-1"
    , "flash"
    , "base-right" ]

defaultMap :: DefaultMap
defaultMap = DefaultMap
    [ "lcdFuncMap" ]

partialMaps :: PartialMaps
partialMaps = PartialMaps [layer1, layer2, layer3]
  where
    layer1 = Layer ["layer-1"]
    layer2 = Layer ["layer-2"]
    layer3 = Layer ["arrow-keys"]



scanModule :: ScanModule
scanModule = ScanModule "MDErgo1"

macroModule :: MacroModule
macroModule = MacroModule "PartialMap"

outputModule :: OutputModule
outputModule = OutputModule "pjrcUSB"

debugModule :: DebugModule
debugModule = DebugModule "full"

chip :: Chip
chip = Chip "mk20dx256vlh7"

compiler :: Compiler
compiler = Compiler "gcc"



newtype BaseMap      = BaseMap [FilePath]
newtype Chip         = Chip String
newtype Compiler     = Compiler FilePath
newtype DebugModule  = DebugModule FilePath
newtype DefaultMap   = DefaultMap [FilePath]
newtype Layer        = Layer [FilePath]
newtype MacroModule  = MacroModule FilePath
newtype OutputModule = OutputModule FilePath
newtype PartialMaps  = PartialMaps [Layer]
newtype ScanModule   = ScanModule FilePath
