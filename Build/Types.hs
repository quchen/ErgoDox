{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}

module Build.Types (

    PrimaryHalf(..),

    BaseMap(..),
    DefaultMap(..),
    PartialMaps(..),
    Layer(..),
    ConfigDependency(..),

    Chip(..),
    Compiler(..),
    DebugModule(..),
    MacroModule(..),
    OutputModule(..),
    ScanModule(..)

) where



import Development.Shake.Classes



data PrimaryHalf = L | R



newtype BaseMap = BaseMap [FilePath]
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
newtype DefaultMap = DefaultMap [FilePath]
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
newtype PartialMaps = PartialMaps [Layer]
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
newtype Layer = Layer [FilePath]
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
newtype ConfigDependency =
    ConfigDependency (Maybe (BaseMap, BaseMap, DefaultMap, PartialMaps))
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)




newtype Chip         = Chip String
newtype Compiler     = Compiler FilePath
newtype DebugModule  = DebugModule FilePath
newtype MacroModule  = MacroModule FilePath
newtype OutputModule = OutputModule FilePath
newtype ScanModule   = ScanModule FilePath


