--
-- Xmonad Configuration by Deni Bertovic
--

import qualified Data.Map                 as M
import           System.IO
import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Config.Gnome
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.NoBorders
import qualified XMonad.StackSet          as W
import           XMonad.Util.EZConfig     (additionalKeys, removeKeys)
import           XMonad.Util.Run          (spawnPipe)
import           XMonad.Util.SpawnOnce


myKeys x = [ ((mod4Mask .|. shiftMask, xK_Return), windows W.swapMaster)
           , ((modMask x, xK_Return), spawn $ XMonad.terminal x) -- %! Launch terminal
           , ((modMask x, xK_Right), nextWS)
           , ((modMask x, xK_Right), nextWS)
           , ((modMask x, xK_Left), prevWS)
           , ((mod4Mask .|. shiftMask, xK_l), spawn "xscreensaver-command -lock")
           ]

newKeys x = M.union (keys changedKeys x) (M.fromList (myKeys x))
    where changedKeys = removeKeys defaultConfig [(mod4Mask .|. shiftMask, xK_Return), (mod4Mask, xK_Return)]

myStartupHook = do
    spawnOnce "xsetroot -cursor_name left_ptr"
    spawnOnce "xmodmap ~/.Xmodmap"
    spawnOnce "gnome-settings-daemon"
    spawnOnce "stalonetray"
    spawnOnce "parcellite"
    spawnOnce "synclient TouchpadOff=1"
    spawnOnce "redshift -c /home/deni/dotfiles/redshift.conf"
    spawnOnce "feh --bg-scale /home/deni/walls/droid.png"
    spawnOnce "xscreensaver -nosplash"
    spawnOnce "dunst"
    spawnOnce "nm-applet"

myManageHook = composeAll
    [ manageHook gnomeConfig
    , resource  =? "stalonetray" --> doIgnore
    , resource  =? "parcellite" --> doIgnore
    , className =? "Gimp"      --> doFloat
    , manageDocks
    ]

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar /home/deni/.xmobarrc"
    xmonad $ gnomeConfig
        { manageHook = myManageHook
        , layoutHook = smartBorders $ avoidStruts $ layoutHook defaultConfig
        , logHook = dynamicLogWithPP xmobarPP
                   { ppOutput = hPutStrLn xmproc
                   , ppTitle = xmobarColor "green" "" . shorten 50
                   }
        , terminal    = "urxvt"
        , startupHook = myStartupHook
        , keys        = newKeys
        , modMask     = mod4Mask -- super key
        }

