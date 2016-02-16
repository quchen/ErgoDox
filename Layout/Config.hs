module Layout.Config (
    baseMap,
    defaultMap,
    partialMaps,
) where



import Build.Types



-- | The default map builds upon the base map. All scancode handling should
-- be done in the base map, so the default map can do re-mappings from keys
-- to keys.
--
-- Since all harware handling is done in the base map, the default map should
-- be independent of your primary/secondary half order.
defaultMap :: DefaultMap
defaultMap = DefaultMap
    [ "lcdFuncMap" ]



-- | Partial maps define the maps activated as long as you hold a modifier down
-- for example.
partialMaps :: PartialMaps
partialMaps = PartialMaps [ layer1 ]
  where
    layer1 = Layer ["arrow-keys", "numpad", "fkeys"]




-- | Base map parametrized over the primary half, which is the one connected
-- to your computer directly.
--
-- The primary purpose of the base map is to get away from the hardware-based
-- scancodes, and translate them to USB codes. It doesn't really matter what
-- you map your scancodes to, as long as the mappings are unique onto USB
-- codes so you can unambiguously redefine them in the base map.
baseMap :: PrimaryHalf -> BaseMap
baseMap primaryHalf = BaseMap (concat
    [ remoteFlashing
    , flashkey
    , firstHalf
    , switchToSlave
    , secondHalf ] )

  where

    -- Mappings for the physical left and right halves.
    rightHalf      = [ "scancode-mapping-right" ]
    leftHalf       = [ "scancode-mapping-left" ]

    -- Enable flashing via remotely connecting to the microcontroller via
    -- a TTY. Sometimes useful, albeit a bit unstable to use for automated
    -- flashing, since the controller likes to be in some sort of broken
    -- state terminal-wise.
    remoteFlashing = [ "enable-remote-flashing" ]

    -- Key combinations to press in order to put the keyboard in flashing mode.
    -- If you forget this and don't have remote flashing enabled, you'll have
    -- to unscrew your ErgoDox again to push the physical button on the back.
    flashkey       = [ "flash-mode-key-combination" ]

    -- Signal the controller that we'd like to access the secondary half's
    -- scancodes.
    switchToSlave  = [ "switch-to-slave-1" ]

    -- Associate the left/right halves with primary/secondary. Flashing the
    -- keyboard the wrong way round will, well, give you a keyboard that's the
    -- wrong way round.
    (firstHalf, secondHalf) = case primaryHalf of
        L -> (leftHalf,  rightHalf)
        R -> (rightHalf, leftHalf)