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
--
-- If you get this wrong the two sides are mirrored. :-)
baseMap :: Half -> BaseMap
baseMap half = BaseMap (always ++ chiral)
  where
    always    = [ "enable-remote-flashing"
                , "flash-mode-key-combination" ]
    rightHalf = [ "scancode-mapping-right" ]
    leftHalf  = [ "scancode-mapping-left" ]
    switch    = [ "switch-to-slave-1" ]

    chiral = case half of
        L -> leftHalf  ++ switch ++ rightHalf
        R -> rightHalf ++ switch ++ leftHalf



defaultMap :: DefaultMap
defaultMap = DefaultMap
    [ "lcdFuncMap" ]



partialMaps :: PartialMaps
partialMaps = PartialMaps [layer1, layer2, layer3, layer4]
  where
    layer1 = Layer ["layer-1"]
    layer2 = Layer ["layer-2"]
    layer3 = Layer ["arrow-keys"]
    layer4 = Layer ["numpad", "fkeys"]
