module Build.WrappedBuildSystemConfig (
    scanModule,
    macroModule,
    outputModule,
    debugModule,
    chip,
    compiler
) where

import Build.Types

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
