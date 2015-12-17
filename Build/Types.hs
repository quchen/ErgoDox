{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE BangPatterns               #-}

module Build.Types (

    PrimaryHalf(..),

    -- * User-facing config
    BaseMap(..),
    DefaultMap(..),
    PartialMaps(..),
    Layer(..),

    -- * Internal config
    Chip(..),
    Compiler(..),
    DebugModule(..),
    MacroModule(..),
    OutputModule(..),
    ScanModule(..)

) where



import GHC.Generics (Generic)

import Development.Shake.Classes


data PrimaryHalf = L | R
    deriving (Eq, Show, Generic)
instance Binary PrimaryHalf
instance NFData PrimaryHalf
instance Hashable PrimaryHalf



newtype BaseMap = BaseMap [FilePath]
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
newtype DefaultMap = DefaultMap [FilePath]
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
newtype PartialMaps = PartialMaps [Layer]
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
newtype Layer = Layer [FilePath]
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)




newtype Chip         = Chip String
newtype Compiler     = Compiler FilePath
newtype DebugModule  = DebugModule FilePath
newtype MacroModule  = MacroModule FilePath
newtype OutputModule = OutputModule FilePath
newtype ScanModule   = ScanModule FilePath


