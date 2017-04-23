--
-- Xmonad Configuration by Deni Bertovic
--

import           Control.Monad
import qualified Data.Map                           as M
import           System.Exit
import           System.IO
import           XMonad
import           XMonad.Actions.Commands
import           XMonad.Actions.CycleWS
import           XMonad.Actions.Volume
import           XMonad.Actions.WithAll             (killAll)
import           XMonad.Config.Gnome
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.BinarySpacePartition
import           XMonad.Layout.Grid
import           XMonad.Layout.IM
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import           XMonad.Prompt                      (XPConfig (..),
                                                     XPPosition (..),
                                                     defaultXPConfig)
import           XMonad.Prompt.ConfirmPrompt
import qualified XMonad.StackSet                    as W
import           XMonad.Util.Dmenu
import qualified XMonad.Util.Dzen                   as D
import           XMonad.Util.EZConfig               (additionalKeys,
                                                     additionalKeysP,
                                                     removeKeys)
import           XMonad.Util.Run                    (spawnPipe)
import           XMonad.Util.SpawnOnce

-- Colors
babyblue  = "#80c0ff"
base0     = "#839496"
base00    = "#657b83"
base01    = "#586e75"
base02    = "#073642"
base03    = "#002b36"
base1     = "#93a1a1"
base2     = "#eee8d5"
base3     = "#fdf6e3"
black     = "#000000"
blue      = "#268bd2"
cyan      = "#2aa198"
darkgray  = "#5f5f5f"
gray      = "#dddddd"
green     = "#859900"
lightgray = "#d3d3d3"
magenta   = "#d33682"
navyblue  = "#000080"
orange    = "#cb4b16"
red       = "#dc322f"
violet    = "#6c71c4"
white     = "#ffffff"
yellow    = "#b58900"

-- sizes
gap         = 10
topbar      = 10
border      = 0
prompt      = 200
status      = 20

myNormalBorderColor     = "#000000"
myFocusedBorderColor    = active

active      = blue
activeWarn  = red
inactive    = base02
focusColor  = blue
unfocusColor = base02

myFont      = "-*-terminus-medium-*-*-*-*-160-*-*-*-*-*-*"
myBigFont   = "-*-terminus-medium-*-*-*-*-240-*-*-*-*-*-*"
myWideFont  = "xft:Eurostar Black Extended:"
            ++ "style=Regular:pixelsize=180:hinting=true"
myPromptFont = "xft:Monospace-Bold:pixelsize=64"

myPromptTheme = def
    { font                  = myPromptFont
    , bgColor               = base03
    , fgColor               = active
    , fgHLight              = base03
    , bgHLight              = active
    , borderColor           = base03
    , promptBorderWidth     = 0
    , height                = prompt
    , position              = Top
    }

warmPromptTheme = myPromptTheme
    { bgColor               = yellow
    , fgColor               = base03
    , position              = Top
    }

hotPromptTheme = myPromptTheme
    { bgColor               = red
    , fgColor               = base3
    , position              = Top
    }

quitPromptTheme = myPromptTheme { promptBorderWidth = 20 }

commands = defaultCommands

myKeys x = [ ((mod4Mask .|. shiftMask, xK_Return), windows W.swapMaster)
           , ((mod4Mask .|. shiftMask, xK_q), confirmPrompt hotPromptTheme "exit" $ io exitSuccess)
           , ((modMask x, xK_Return), spawn $ XMonad.terminal x) -- %! Launch terminal
           , ((modMask x, xK_Right), nextWS)
           , ((modMask x, xK_Right), nextWS)
           , ((modMask x, xK_Left), prevWS)
           , ((mod4Mask .|. controlMask, xK_BackSpace), confirmPrompt hotPromptTheme "kill all" $ killAll)
           , ((mod4Mask .|. controlMask, xK_y), commands >>= runCommand)
           , ((modMask x, xK_Print), spawn "gnome-screenshot")
           , ((mod4Mask .|. shiftMask, xK_Print), spawn "gnome-screenshot --interactive")
           , ((mod4Mask .|. shiftMask, xK_l), spawn "xscreensaver-command -lock")
           ]

micToggleCmd = "amixer -q set Capture toggle && amixer get Capture | grep '\\[off\\]' && notify-send \"MIC OFF\" || notify-send \"MIC ON\""

specialKeys = [ ("<XF86AudioMute>",         toggleMute >>= showAudioMuteAlert)
              , ("<XF86AudioLowerVolume>",  lowerVolume 4 >>= alert)
              , ("<XF86AudioRaiseVolume>",  raiseVolume 4 >>= alert)
              , ("<XF86AudioMicMute>",      spawn micToggleCmd)
              , ("<XF86MonBrightnessUp>",   spawn "/home/deni/scripts/brightness.sh +10")
              , ("<XF86MonBrightnessDown>", spawn "/home/deni/scripts/brightness.sh -10")
              ]

newKeys x = M.union (keys changedKeys x) (M.fromList (myKeys x))
    where changedKeys = removeKeys defaultConfig [(mod4Mask .|. shiftMask, xK_Return), (mod4Mask, xK_Return), (mod4Mask .|. shiftMask,   xK_q)]

alert = D.dzenConfig (centered 150) . show . round
centered w =
        D.onCurr (D.center w 66)
    >=> D.font "-*-helvetica-*-r-*-*-64-*-*-*-*-*-*-*"
    >=> D.addArgs ["-fg", lightgray]
    >=> D.addArgs ["-bg", black]

showAudioMuteAlert True  = D.dzenConfig (centered 300) $ "Sound Off"
showAudioMuteAlert False = D.dzenConfig (centered 300) $ "Sound On"

myStartupHook = do
    spawnOnce "xsetroot -cursor_name left_ptr"
    spawnOnce "xmodmap ~/.Xmodmap"
    spawnOnce "synclient TouchpadOff=1"
    spawnOnce "feh --bg-scale /home/deni/walls/droid.png"

myManageHook = composeAll
    [ manageHook gnomeConfig
    , resource  =? "parcellite"       --> doIgnore
    , className =? "Gimp"             --> doFloat
    , className =? "Gnome-calculator" --> doFloat
    , className =? "Keybase"          --> doFloat
    , className =? "Totem"            --> doFloat
    , className =? "Keepassx"         --> doFloat
    , className =? "SpiderOakONE"     --> doFloat
    , className =? "Gnome-Screenshot" --> doIgnore
    , className =? "Pidgin"           --> doShift "3"
    , classNotRole ("Pidgin", "buddy_list") --> doFloat
    , manageDocks
    ]
  where classNotRole :: (String, String) -> Query Bool
        classNotRole (c,r) = (className =? c) <&&> (role /=? r)
        role = stringProperty "WM_WINDOW_ROLE"


-- Layouts
myLayout = smartBorders $ avoidStruts $ onWorkspace "3" imLayout $ myLayouts
  where
    --          numMasters, resizeIncr, splitRatio
    tall = Tall 1           0.02        0.5
    -- define the list of standardLayouts
    myLayouts = tall ||| Mirror tall ||| Full ||| Grid ||| emptyBSP
    -- notice that withIM, which normally acts on one layout, can also
    -- work on a list of layouts (yay recursive data types!)
    imLayout = withIM (2/10) (Role "buddy_list") myLayouts

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar /home/deni/.xmobarrc"
    xmonad $ gnomeConfig
        { manageHook = myManageHook
        , layoutHook = myLayout
        , logHook = dynamicLogWithPP xmobarPP
                   { ppOutput = hPutStrLn xmproc
                   , ppTitle = xmobarColor "green" "" . shorten 50
                   }
        , terminal    = "urxvt"
        , startupHook = myStartupHook
        , keys        = newKeys
        , modMask     = mod4Mask -- super key
        } `additionalKeysP` specialKeys

