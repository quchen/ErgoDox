module Layout.Config (
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
    compiler
) where



import Build.Types



-- | Base map parametrized over the primary half. If you get this wrong, the
-- two sides are mirrored. :-)
baseMap :: Half -> BaseMap
baseMap half = BaseMap (always ++ chiral)
  where
    always    = [ "flash-remote" ]
    rightHalf = [ "flash", "base-right" ]
    leftHalf  = [ "flash", "base-left" ]
    switch    = [ "switch-to-slave-1" ]

    chiral = case half of
        L -> leftHalf  ++ switch ++ rightHalf
        R -> rightHalf ++ switch ++ leftHalf

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



