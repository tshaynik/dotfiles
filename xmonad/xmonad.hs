import XMonad

import qualified XMonad.Actions.Search as S

import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import Graphics.X11.ExtraTypes.XF86
import System.IO (hPutStrLn)

-- Use "Windows" key
myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "kitty"

myNormalColor :: String
myNormalColor = "#282c34" -- Border colour for normal windows

myFocusColor :: String
myFocusColor  = "#8252c9"  -- Border colour of focused windows

myBorderWidth :: Int
myBorderWidth = 2

-------------------

myBar = "xmobar"

-- pretty printing of xmobar
myPP = xmobarPP { ppTitle = xmobarColor myFocusColor ""
                , ppCurrent = xmobarColor "#429942" "" . wrap "<" ">"
                }

-- key binding to toggle gap for bar
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

------------------

myDesktop = desktopConfig
    { terminal = myTerminal
    -- Key remapping
    , modMask  = myModMask

    , manageHook = manageDocks <+> manageHook def
    , layoutHook = smartBorders . avoidStruts $ layoutHook def
    , logHook = return ()
    , normalBorderColor = myNormalColor
    , focusedBorderColor = myFocusColor
    , borderWidth = 2
    }

myKeys =
  [ ((0, xK_Print),
      spawn "scrot -z -e 'mv $f ~/Pictures'") -- Screenshots

  , ((myModMask .|. shiftMask, xK_l),
      spawn "slock")

  -- Volume
  -- Mute volume.
  , ((0, xF86XK_AudioMute),
       spawn "amixer -q set Master toggle")
  -- Decrease volume.
  , ((0, xF86XK_AudioLowerVolume),
       spawn "amixer -q set Master 5%-")
  -- Increase volume.
  , ((0, xF86XK_AudioRaiseVolume),
       spawn "amixer -q set Master 5%+")

  -- App shortcuts
  , ((myModMask, xK_F1),
        spawn "firefox")
  , ((myModMask, xK_F2),
        spawn "firefox --private-window")
  , ((myModMask, xK_F3),
        spawn "pcmanfm")

  , ((myModMask, xK_F5),
        spawn "xrandr --auto")
  , ((myModMask, xK_F6),
        spawn "xrandr --auto; xrandr --output eDP-1 --off")

  -- Keyboard Layouts
   , ((myModMask, xK_F9),
        spawn "setxkbmap gr")
 , ((myModMask, xK_F10),
        spawn "setxkbmap il")
  , ((myModMask, xK_F11),
        spawn "setxkbmap ca")
  , ((myModMask, xK_F12),
        spawn "setxkbmap us")
  ]

main = do
    spawn "stalonetray"
    spawn "volumeicon"
    xmonad =<< statusBar myBar myPP toggleStrutsKey (myDesktop `additionalKeys` myKeys)
