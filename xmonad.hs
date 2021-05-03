import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig

main = xmonad $ docks $ fullscreenSupport def
    { modMask = mod4Mask
    , terminal = "termite"
    , borderWidth = 3
    , normalBorderColor  = "#282828"
    , focusedBorderColor = "#b8bb26"
    , startupHook     = spawn "picom -b"
    , layoutHook      = myLayoutHook
    , handleEventHook = fullscreenEventHook
    , manageHook      = fullscreenManageHook
    } `additionalKeysP`
        [ ("<Print>", spawn "flameshot gui")
        , ("M-s", spawn "rofi -show drun")
        , ("<XF86AudioLowerVolume>", spawn "pamixer -d 7")
        , ("<XF86AudioRaiseVolume>", spawn "pamixer -i 7")
        ]

myLayoutHook = smartBorders $ avoidStruts $
    noBorders Full ||| tall
    where
        tall = Tall 1 0.1 0.5
