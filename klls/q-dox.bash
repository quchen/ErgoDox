#!/usr/bin/env bash
# David Luposchainsky 2015



ScanModule="MDErgo1"
MacroModule="PartialMap"
OutputModule="pjrcUSB"
DebugModule="full"
Chip="mk20dx256vlh7"
Compiler="gcc"
if [ ! -f cmake.bash ]; then
    echo "ERROR: Cannot find 'cmake.bash'"
    exit 1
fi



# Left half primary

BuildPath="ergodox-left"
# BaseMap="defaultMap leftHand slave1 rightHand"
BaseMap="q-base-left q-flash-remote q-flash q-switch-to-slave-1 q-flash q-base-right"
DefaultMap="lcdFuncMap"
PartialMaps[1]="q-layer-1"
PartialMaps[2]="q-layer-2"
PartialMaps[3]="q-arrow-keys"
PartialMaps[4]="q-layer-2"
PartialMaps[5]="q-layer-2"
PartialMaps[6]="q-layer-2"
PartialMaps[7]="q-layer-2"
PartialMaps[8]="q-layer-2"
# PartialMaps[1]="iced_func"
# PartialMaps[2]="iced_numpad"
source cmake.bash




# Right half primary

BuildPath="ergodox-right"
BaseMap="q-base-right q-flash-remote q-flash q-switch-to-slave-1 q-flash q-base-left"
source cmake.bash
