import           Control.Monad                  ( join
                                                , when
                                                )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( maybeToList )
import           XMonad                  hiding ( (|||) )
import           XMonad.Hooks.EwmhDesktops
                                         hiding ( fullscreenEventHook )
import           XMonad.Hooks.FadeInactive
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.BinarySpacePartition
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.Gaps
import           XMonad.Layout.LayoutCombinators
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Spacing
import           XMonad.Layout.StateFull
import           XMonad.Operations
import qualified XMonad.StackSet               as StackSet
import           XMonad.Util.EZConfig
import           XMonad.Util.SpawnOnce

main :: IO ()
main = xmonad $ docks $ ewmh def
    { modMask         = mod4Mask
    , terminal        = "kitty"
    , borderWidth     = 0
    , startupHook     = myStartupHook <+> setWMName "LG3D"
    , layoutHook      = myLayoutHook
    , logHook         = myLogHook
    , handleEventHook = fullscreenEventHook
    , manageHook      = manageHook def <+> fullscreenManageHook <+> myManageHook
    , keys            = \x -> myKeybindings x `Map.union` Map.fromList
                            (filter (flip notElem (defaultKeybindingsToRemove x) . fst) (Map.toList (keys def x)))
    , mouseBindings   = myMouseBindings
    }

addNETSupported x = withDisplay $ \dpy -> do
    r               <- asks theRoot
    a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
    a               <- getAtom "ATOM"
    liftIO $ do
        sup <- join . maybeToList <$> getWindowProperty32 dpy a_NET_SUPPORTED r
        when (fromIntegral x `notElem` sup) $ changeProperty32 dpy r a_NET_SUPPORTED a propModeAppend [fromIntegral x]
addEWMHFullscreen = do
    wms <- getAtom "_NET_WM_STATE"
    wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
    mapM_ addNETSupported [wms, wfs]

myStartupHook = do
    spawn "killall picom; picom --experimental-backends -b"
    spawn "killall dzen2; /home/archbox/bin/start-dzen2 &"
    addEWMHFullscreen

myLayoutHook =
    gaps [(U, 24)]
        $   spacingRaw True (Border 8 8 8 8) True (Border 8 8 8 8) True
        $   smartBorders
        $   avoidStruts
        $   noBorders StateFull
        ||| tall
        ||| emptyBSP
    where tall = Tall 1 0.1 0.5
layouts = ["StateFull", "Tall", "BinarySpacePartition"]

myManageHook = composeAll [isFullscreen --> doFullFloat, title =? "Picture-in-Picture" --> doFloat]

myKeybindings x = mkKeymap
    x
    [ ("<Print>"               , spawn "flameshot gui")
    , ("M-p"                   , spawn "flameshot gui")
    , ("M-s"                   , spawn "rofi -show drun")
    , ("M-S-s", spawn "rofi -modi 'clipboard:greenclip print' -show clipboard -run-command '{cmd}'")
    , ("M-e", spawn "rofi -modi 'emoji:rofimoji --action copy' -show emoji")
    , ("M-t"                   , spawn $ terminal x)
    , ("<XF86AudioLowerVolume>", spawn "pamixer -d 5" >> spawn "sleep 0.1; statusbar-scripts/send-volume.py $(pamixer --get-volume)")
    , ("<XF86AudioRaiseVolume>", spawn "pamixer -i 5" >> spawn "sleep 0.1; statusbar-scripts/send-volume.py $(pamixer --get-volume)")
    , ("M-M1-l"                , spawn "xscreensaver-command -lock")
    , ("M-j"                   , sendMessage Shrink)
    , ("M-k"                   , windows StackSet.focusUp)
    , ("M-l"                   , windows StackSet.focusDown)
    , ("M-;"                   , sendMessage Expand)
    , ("M-q", spawn "killall lemonbar picom; xmonad --recompile; xmonad --restart")
    , ("M-S-k"                 , windows StackSet.swapUp)
    , ("M-S-l"                 , windows StackSet.swapDown)
    , ("M-S-t"                 , withFocused $ windows . StackSet.sink)
    , ("M-x"                   , sendMessage $ JumpToLayout "StateFull")
    , ("M-c"                   , sendMessage $ JumpToLayout "Tall")
    , ("M-v", sendMessage $ JumpToLayout "BinarySpacePartition")
    ]

defaultKeybindingsToRemove x =
    [ (modMask x .|. shiftMask, xK_Return) -- new terminal, replace with mod+t
    , (modMask x              , xK_h) -- remap hjkl to jkl;
    , (modMask x              , xK_j)
    , (modMask x              , xK_k)
    , (modMask x              , xK_l)
    , (modMask x              , xK_q) -- remove to add more stuff to xmonad restart
    , (modMask x .|. shiftMask, xK_h)
    , (modMask x .|. shiftMask, xK_j)
    , (modMask x .|. shiftMask, xK_k)
    , (modMask x .|. shiftMask, xK_l)
    , (modMask x              , xK_t)
    , (modMask x              , xK_p)
    , (modMask x              , xK_space)
    ]

myMouseBindings x =
    Map.fromList
        $ [ ((modMask x, button1), (\w -> focus w >> mouseMoveWindow w))
          , ((modMask x, button2), (\w -> focus w >> windows StackSet.shiftMaster))
          , ((modMask x, button3), (\w -> focus w >> mouseResizeWindow w))
          ]

myLogHook = fadeOutLogHook $ fadeIf (not <$> (title =? "Picture-in-Picture") <&&> isUnfocused) 0.8
