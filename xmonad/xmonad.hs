import XMonad

import qualified XMonad.Actions.Search as S
import qualified XMonad.Actions.Submap as SM

import XMonad.Config.Desktop

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks

import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen

import qualified XMonad.Prompt as P

import qualified XMonad.StackSet as W

import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.SpawnOnce
import qualified XMonad.Util.NamedScratchpad as NS

import qualified DBus as D
import qualified DBus.Client as DC

import qualified Codec.Binary.UTF8.String as UTF8
import qualified Data.Map as M
import Graphics.X11.ExtraTypes.XF86
import System.IO (hPutStrLn)

-- Use "Windows" key
myModMask :: KeyMask
myModMask = mod4Mask

myFont :: String
myFont = "xft:FiraCode:bold:size=12:antialias=true:hinting=true"

myTerminal :: String
myTerminal = "kitty"

myNormalColor :: String
myNormalColor = "#282c34" -- Border colour for normal windows

myFocusColor :: String
myFocusColor  = "#8252c9"  -- Border colour of focused windows

myBorderWidth :: Int
myBorderWidth = 2

-- Colours
fg        = "#ebdbb2"
bg        = "#282828"
gray      = "#a89984"
bg1       = "#3c3836"
bg2       = "#505050"
bg3       = "#665c54"
bg4       = "#7c6f64"

green     = "#b8bb26"
darkgreen = "#98971a"
red       = "#fb4934"
darkred   = "#cc241d"
yellow    = "#fabd2f"
blue      = "#83a598"
purple    = "#d3869b"
aqua      = "#8ec07c"
white     = "#eeeeee"

pur2      = "#5b51c9"
blue2     = "#2266d0"

myLogHook :: DC.Client -> PP
myLogHook dbus = def
        { ppOutput = dbusOutput dbus
        , ppCurrent = wrap ("%{F" ++ blue2 ++ "} ") " %{F-}"
        , ppVisible = wrap ("%{F" ++ blue ++ "} ") " %{F-}"
        , ppUrgent = wrap ("%{F" ++ red ++ "} ") " %{F-}"
        , ppHidden = wrap " " " "
        , ppWsSep = ""
        , ppSep = " | "
        --, ppTitle = myAddSpaces 25
        }
    where
        -- Emit a DBus signal on log updates
        dbusOutput :: DC.Client -> String -> IO ()
        dbusOutput dbus str = do
            let signal = (D.signal objectPath interfaceName memberName) {
                    D.signalBody = [D.toVariant $ UTF8.decodeString str]
                }
            DC.emit dbus signal

        objectPath = D.objectPath_ "/org/xmonad/Log"
        interfaceName = D.interfaceName_ "org.xmonad.Log"
        memberName = D.memberName_ "Update"

myDesktop = desktopConfig
    { terminal = myTerminal
    -- Key remapping
    , modMask  = myModMask

    , manageHook = manageDocks <+> manageHook def
    , layoutHook = smartBorders . avoidStruts $ layoutHook def
    , normalBorderColor = myNormalColor
    , focusedBorderColor = myFocusColor
    , borderWidth = 2
    , startupHook = myStartupHook
    }

myStartupHook = do
  spawn "$HOME/.config/polybar/launch.sh"

myScratchPads = [ NS.NS "terminal" (myTerminal <> " --title scratchpad") (title =? "scratchpad") manageTerm
                , NS.NS "htop" (myTerminal <> " htop") (title =? "htop") manageTerm
                ]
  where
    manageTerm = NS.customFloating $ W.RationalRect l t w h
      where
              h = 0.9
              w = 0.9
              t = 0.95 - h
              l = 0.95 - w

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
  , ((myModMask, xK_z),
        spawn "EDITOR=nvim kitty neuron -d ~/zettelkasten new -e")

  -- Keyboard Layouts
  , ((myModMask, xK_F9),
        spawn "setxkbmap gr")
  , ((myModMask, xK_F10),
        spawn "setxkbmap il")
  , ((myModMask, xK_F11),
        spawn "setxkbmap ca")
  , ((myModMask, xK_F12),
        spawn "setxkbmap us")

  -- Layouts
  , ((myModMask, xK_b),
        sendMessage ToggleStruts)

  -- search
  , ((myModMask, xK_s), SM.submap $ searchMap $ S.promptSearch dtXPConfig')
  , ((myModMask .|. shiftMask, xK_s), SM.submap $ searchMap $ S.selectSearch)

  -- scratchpads
  , ((myModMask .|. controlMask, xK_Return), NS.namedScratchpadAction myScratchPads "terminal")
  , ((myModMask .|. controlMask .|. shiftMask, xK_h), NS.namedScratchpadAction myScratchPads "htop")
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
        , ((0, xK_c), method $ S.searchEngine "crates.io" "https://crates.io/search?q=")
        , ((0, xK_d), method S.duckduckgo)
        , ((0, xK_g), method S.google)
        , ((0, xK_n), method $ S.searchEngine "nixos packages" "https://search.nixos.org/packages?query=")
        , ((0, xK_p), method $ S.searchEngine "nixos options" "https://search.nixos.org/options?query=")
        , ((0, xK_s), method $ S.searchEngine "spotify" "https://open.spotify.com/search/")
        , ((0, xK_y), method S.youtube)
        , ((0, xK_w), method S.wikipedia)
        ]

main = do
    dbus <- DC.connectSession
    DC.requestName dbus (D.busName_ "org.xmonad.Log") [DC.nameAllowReplacement, DC.nameReplaceExisting, DC.nameDoNotQueue]

    xmonad
      $ ewmh
      $ myDesktop {logHook = dynamicLogWithPP (myLogHook dbus)} `additionalKeys` myKeys
