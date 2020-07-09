import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import Graphics.X11.ExtraTypes.XF86
import System.IO

-- Use "Windows" key
myModMask = mod4Mask

main = do
    spawn "stalonetray"
    spawn "volumeicon"
    spawn "nm-applet"
    xmproc <- spawnPipe "xmobar ~/.xmobarrc"
    xmonad $ myDesktop xmproc `additionalKeys` myKeys

myDesktop xmproc = desktopConfig
      { terminal = "kitty"
      -- Key remapping
      , modMask  = myModMask

      , manageHook = manageDocks <+> manageHook defaultConfig
      , layoutHook = smartBorders . avoidStruts $ layoutHook defaultConfig
      , logHook = dynamicLogWithPP xmobarPP
                  { ppOutput = hPutStrLn xmproc
                  , ppTitle = xmobarColor "green" "" . shorten 50
                  }
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
