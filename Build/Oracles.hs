{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Build.Oracles (
    oracles,

    baseMapConfig,
    defaultMapConfig,
    partialMapsConfig
) where



import Development.Shake
import Development.Shake.Classes

import Build.Types
import Layout.Config



oracles :: Rules ()
oracles = mconcat [ baseMapConfigOracle
                  , defaultMapConfigOracle
                  , partialMapsConfigOracle ]



newtype BaseMapQ = BaseMapQ PrimaryHalf
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

baseMapConfig :: PrimaryHalf -> Action BaseMap
baseMapConfig primaryHalf = askOracle (BaseMapQ primaryHalf)

baseMapConfigOracle :: Rules ()
baseMapConfigOracle = () <$
    addOracle (\(BaseMapQ primaryHalf) -> pure (baseMap primaryHalf))



newtype DefaultMapQ = DefaultMapQ ()
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

defaultMapConfig :: Action DefaultMap
defaultMapConfig = askOracle (DefaultMapQ ())

defaultMapConfigOracle :: Rules ()
defaultMapConfigOracle = () <$
    addOracle (\(DefaultMapQ _) -> pure defaultMap)



newtype PartialMapsQ = PartialMapsQ ()
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

partialMapsConfig :: Action PartialMaps
partialMapsConfig = askOracle (PartialMapsQ ())

partialMapsConfigOracle :: Rules ()
partialMapsConfigOracle = () <$
    addOracle (\(PartialMapsQ _) -> pure partialMaps)
