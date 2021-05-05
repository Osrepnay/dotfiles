import qualified Data.Map as Map
import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Util.EZConfig

main :: IO ()
main = xmonad $ docks $ fullscreenSupport def
    { modMask = mod4Mask
    , terminal = "termite"
    , borderWidth = 2
    , normalBorderColor  = "#282828"
    , focusedBorderColor = "#d79921"
    , layoutHook      = myLayoutHook
    , handleEventHook = fullscreenEventHook
    , manageHook      = fullscreenManageHook
    , keys = \x -> myKeybindings x
        `Map.union`
        Map.fromList (filter
            (((flip notElem) (defaultKeybindingsToRemove x)) . fst)
            (Map.toList ((keys def) x)))
    }

myLayoutHook =
    gaps [(U, 24)] $
    spacingRaw True (Border 8 8 8 8) True (Border 8 8 8 8) True $
    smartBorders $
    avoidStruts $
    noBorders Full ||| tall
    where
        tall = Tall 1 0.1 0.5

myKeybindings x = mkKeymap x
    [ ("<Print>", spawn "flameshot gui")
    , ("M-s", spawn "rofi -show drun")
    , ("M-t", spawn $ terminal x)
    , ("<XF86AudioLowerVolume>", spawn "pamixer -d 7")
    , ("<XF86AudioRaiseVolume>", spawn "pamixer -i 7")
    ]

defaultKeybindingsToRemove x =
    [(modMask x .|. shiftMask, xK_Return)] -- new terminal, replace with mod+t
