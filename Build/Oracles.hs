{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Build.Oracles (
    oracles,
    dependOnConfig
) where



import Development.Shake
import Development.Shake.Classes

import Build.Types
import Layout.Config




oracles :: Rules ()
oracles = dependOnConfigOracle



newtype ConfigDependencyQ = ConfigDependencyQ PrimaryHalf
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
newtype ConfigDependencyA = ConfigDependencyA (BaseMap, DefaultMap, PartialMaps)
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

-- Add artificial dependencies on the configuration to rebuild when it
-- changes
dependOnConfig :: PrimaryHalf -> Action ()
dependOnConfig primaryHalf= do
    ConfigDependencyA _ <- askOracle (ConfigDependencyQ primaryHalf)
    pure ()

-- | Oracle to depend on the configuration (as in Config.hs). This is used
-- to trigger rebuilds when layouts are changed without touching the KLLs,
-- for example when layers are swapped.
dependOnConfigOracle :: Rules ()
dependOnConfigOracle = () <$
    addOracle (\(ConfigDependencyQ primaryHalf) -> pure (
        ConfigDependencyA ( baseMap primaryHalf
                          , defaultMap
                          , partialMaps )))
