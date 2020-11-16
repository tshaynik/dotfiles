import XMonad

import qualified XMonad.Actions.Search as S
import qualified XMonad.Actions.Submap as SM

import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen
import qualified XMonad.Prompt as P
import XMonad.Util.EZConfig(additionalKeys)

import qualified Data.Map as M
import Graphics.X11.ExtraTypes.XF86
import System.IO (hPutStrLn)

-- Use "Windows" key
myModMask :: KeyMask
myModMask = mod4Mask

myFont :: String
myFont = "xft:Fira Code:bold:size=9:antialias=true:hinting=true"


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

  -- search
  , ((myModMask, xK_s), SM.submap $ searchMap $ S.promptSearch dtXPConfig')
  , ((myModMask .|. shiftMask, xK_s), SM.submap $ searchMap $ S.selectSearch)
  ]

dtXPConfig :: P.XPConfig
dtXPConfig = def
      { P.font                = myFont
      , P.bgColor             = "#282c34"
      , P.fgColor             = "#bbc2cf"
      , P.bgHLight            = "#c792ea"
      , P.fgHLight            = "#000000"
      , P.borderColor         = "#535974"
      , P.promptBorderWidth   = 0
      --, promptKeymap        = dtXPKeymap
      , P.position            = P.Top
     -- , position            = CenteredAt { xpCenterY = 0.3, xpWidth = 0.3 }
      , P.height              = 20
      , P.historySize         = 256
      , P.historyFilter       = id
      , P.defaultText         = []
      , P.autoComplete        = Just 100000  -- set Just 100000 for .1 sec
      , P.showCompletionOnTab = False
      -- , searchPredicate     = isPrefixOf
      --, searchPredicate     = fuzzyMatch
      --, defaultPrompter     = id $ map toUpper  -- change prompt to UPPER
      -- , defaultPrompter     = unwords . map reverse . words  -- reverse the prompt
      -- , defaultPrompter     = drop 5 .id (++ "XXXX: ")  -- drop first 5 chars of prompt and add XXXX:
      , P.alwaysHighlight     = True
      , P.maxComplRows        = Nothing      -- set to 'Just 5' for 5 rows
      }

-- The same config above minus the autocomplete feature which is annoying
-- on certain Xprompts, like the search engine prompts.
dtXPConfig' :: P.XPConfig
dtXPConfig' = dtXPConfig
      { P.autoComplete        = Nothing
      }

searchMap method = M.fromList $
        [ ((0, xK_a), method $ S.searchEngine "archwiki" "https://wiki.archlinux.org/index.php?search=")
        , ((0, xK_d), method S.duckduckgo)
        , ((0, xK_g), method S.google)
        , ((0, xK_n), method $ S.searchEngine "nixos packages" "https://search.nixos.org/packages?query=")
        , ((0, xK_p), method $ S.searchEngine "nixos options" "https://search.nixos.org/options?query=")
        , ((0, xK_y), method S.youtube)
        , ((0, xK_w), method S.wikipedia)
        ]

main = do
    spawn "stalonetray"
    spawn "volumeicon"
    xmonad =<< statusBar myBar myPP toggleStrutsKey (myDesktop `additionalKeys` myKeys)
