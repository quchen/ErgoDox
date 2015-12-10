module Layout.Config (
    baseMap,
    defaultMap,
    partialMaps,
) where



import Build.Types



-- | Base map parametrized over the primary half, i.e. the one connected
-- to your computer directly.
--
-- The base layout usually consists of mapping scancodes like 0x1A to
-- USB codes like U"Esc".
baseMap :: PrimaryHalf -> BaseMap
baseMap primaryHalf = BaseMap (remoteFlashing : firstHalf ++ secondHalf)
  where
    rightHalf      = [ "scancode-mapping-right" ]
    leftHalf       = [ "scancode-mapping-left" ]

    remoteFlashing = "enable-remote-flashing"
    flashkey       = "flash-mode-key-combination"
    switchToSlave  = "switch-to-slave-1"
    (firstHalf, secondHalf) = case primaryHalf of
        L -> (flashkey : leftHalf,  switchToSlave : rightHalf)
        R -> (flashkey : rightHalf, switchToSlave : leftHalf)



defaultMap :: DefaultMap
defaultMap = DefaultMap
    [ "lcdFuncMap" ]



partialMaps :: PartialMaps
partialMaps = PartialMaps [ layer1 ]
  where
    layer1 = Layer ["arrow-keys", "numpad", "fkeys"]
