module Build.Types (

    Half(..),

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



-- | Left/right Ergodox half identifier
data Half = L | R

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


