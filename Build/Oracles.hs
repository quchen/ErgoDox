{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Build.Oracles (
    oracles,
    keyboardConfig
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
keyboardConfig :: PrimaryHalf -> Action (BaseMap, DefaultMap, PartialMaps)
keyboardConfig primaryHalf = do
    ConfigDependencyA x <- askOracle (ConfigDependencyQ primaryHalf)
    pure x

-- | Oracle to depend on the configuration (as in Config.hs). This is used
-- to trigger rebuilds when layouts are changed without touching the KLLs,
-- for example when layers are swapped.
dependOnConfigOracle :: Rules ()
dependOnConfigOracle = () <$
    addOracle (\(ConfigDependencyQ primaryHalf) -> pure (
        ConfigDependencyA ( baseMap primaryHalf
                          , defaultMap
                          , partialMaps )))
