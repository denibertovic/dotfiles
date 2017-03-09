--
-- Xmonad Configuration by Deni Bertovic
--

import qualified Data.Map                   as M
import           System.IO
import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Config.Gnome
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.IM
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import qualified XMonad.StackSet            as W
import           XMonad.Util.EZConfig       (additionalKeys, additionalKeysP,
                                             removeKeys)
import           XMonad.Util.Run            (spawnPipe)
import           XMonad.Util.SpawnOnce


myKeys x = [ ((mod4Mask .|. shiftMask, xK_Return), windows W.swapMaster)
           , ((modMask x, xK_Return), spawn $ XMonad.terminal x) -- %! Launch terminal
           , ((modMask x, xK_Right), nextWS)
           , ((modMask x, xK_Right), nextWS)
           , ((modMask x, xK_Left), prevWS)
           , ((modMask x, xK_Print), spawn "gnome-screenshot")
           , ((mod4Mask .|. shiftMask, xK_Print), spawn "gnome-screenshot --interactive")
           , ((mod4Mask .|. shiftMask, xK_l), spawn "xscreensaver-command -lock")
           ]

audioToggleCmd = "amixer -q set Master toggle && amixer get Master | grep '\\[off\\]' && notify-send \"AUDIO OFF\" || notify-send \"AUDIO ON\""

micToggleCmd = "amixer -q set Capture toggle && amixer get Capture | grep '\\[off\\]' && notify-send \"MIC OFF\" || notify-send \"MIC ON\""

specialKeys = [ ("<XF86AudioMute>",         spawn audioToggleCmd)
              , ("<XF86AudioLowerVolume>",  spawn "amixer -c 0 set Master 4dB-")
              , ("<XF86AudioRaiseVolume>",  spawn "amixer -c 0 set Master 4dB+")
              , ("<XF86AudioMicMute>",      spawn micToggleCmd)
              , ("<XF86MonBrightnessUp>",   spawn "/home/deni/scripts/brightness.sh +10")
              , ("<XF86MonBrightnessDown>", spawn "/home/deni/scripts/brightness.sh -10")
              ]

newKeys x = M.union (keys changedKeys x) (M.fromList (myKeys x))
    where changedKeys = removeKeys defaultConfig [(mod4Mask .|. shiftMask, xK_Return), (mod4Mask, xK_Return)]

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
myLayout = smartBorders $ avoidStruts $ onWorkspace "3" imLayout $ standardLayouts
  where
    --          numMasters, resizeIncr, splitRatio
    tall = Tall 1           0.02        0.5
    -- define the list of standardLayouts
    standardLayouts = layoutHook defaultConfig -- tall ||| Mirror tall ||| Full
    -- notice that withIM, which normally acts on one layout, can also
    -- work on a list of layouts (yay recursive data types!)
    imLayout = withIM (2/10) (Role "buddy_list") standardLayouts

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar /home/deni/.xmobarrc"
    xmonad $ gnomeConfig
        { manageHook = myManageHook
        , layoutHook = myLayout -- smartBorders $ avoidStruts $ layoutHook defaultConfig
        , logHook = dynamicLogWithPP xmobarPP
                   { ppOutput = hPutStrLn xmproc
                   , ppTitle = xmobarColor "green" "" . shorten 50
                   }
        , terminal    = "urxvt"
        , startupHook = myStartupHook
        , keys        = newKeys
        , modMask     = mod4Mask -- super key
        } `additionalKeysP` specialKeys

