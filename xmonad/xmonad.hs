{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses #-}
--
-- Xmonad Configuration by Deni Bertovic
--

import           Control.Monad
import           Data.List                          (isInfixOf, isPrefixOf)
import qualified Data.Map                           as M
import           Data.Monoid                        ((<>))
import           System.Exit
import           System.IO
import           System.Process hiding (runCommand)
import           XMonad
import           XMonad.Actions.Commands
import           XMonad.Actions.CopyWindow          (copyToAll,
                                                     killAllOtherCopies,
                                                     wsContainingCopies)
import           XMonad.Actions.CycleWS
import           Graphics.X11.Xinerama              (getScreenInfo)
import           XMonad.Hooks.EwmhDesktops hiding   (fullscreenEventHook)
import           XMonad.Actions.DynamicProjects
import           XMonad.Actions.DynamicWorkspaces   (withNthWorkspace)
import           XMonad.Actions.SpawnOn             (spawnOn, manageSpawn)
import           XMonad.Actions.Volume
import           XMonad.Actions.WindowBringer       (bringMenu, gotoMenu)
import           XMonad.Actions.WithAll             (killAll)
import           XMonad.Config.Gnome
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.InsertPosition
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.UrgencyHook
-- import           XMonad.Hooks.Place                  (simpleSmart, placeFocused, placeHook, inBounds, underMouse)
import           XMonad.Layout.Accordion            (Accordion (..))
import           XMonad.Layout.Renamed
import           XMonad.Layout.Fullscreen           (fullscreenFloat, fullscreenManageHook
                                                    , fullscreenEventHook)
import           XMonad.Hooks.FadeWindows           (fadeWindowsEventHook, fadeWindowsLogHook
                                                    , opaque, opacity, isUnfocused)
import           XMonad.Layout.BinarySpacePartition
import           XMonad.Layout.BoringWindows
import           XMonad.Layout.Grid
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.Spacing
import           XMonad.Layout.Hidden               (hideWindow,
                                                     popOldestHiddenWindow)
import           XMonad.Layout.IM
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Simplest             (Simplest (..))
import           XMonad.Layout.NoFrillsDecoration   (noFrillsDeco)
import           XMonad.Layout.SubLayouts
import           XMonad.Layout.Tabbed               (CustomShrink (..),
                                                     Shrinker (..), Theme (..),
                                                     addTabs, addTabsBottom, shrinkText,
                                                     simpleTabbed, tabbed)
import           XMonad.Layout.WindowNavigation
import           XMonad.Actions.Navigation2D        (screenGo, windowToScreen, screenSwap)
import           XMonad.ManageHook
import           XMonad.Prompt                      (XPConfig (..),
                                                     XPPosition (..),
                                                     defaultXPConfig)
import           XMonad.Prompt.ConfirmPrompt
import           XMonad.Prompt.Shell                (shellPrompt)
import qualified XMonad.StackSet                    as W
import           XMonad.StackSet                    (StackSet(..), Screen(..))
import qualified XMonad.Util.Dzen                   as D
import           XMonad.Util.EZConfig               (additionalKeys,
                                                     additionalKeysP,
                                                     removeKeys,
                                                     removeKeysP,
                                                     mkNamedKeymap)
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.NamedWindows    (getName)
import           XMonad.Util.NamedActions
import           XMonad.Util.Run                    (runInTerm, spawnPipe, safeSpawnProg, safeSpawn)
import           XMonad.Util.SpawnOnce


myTerminal = "urxvt"

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
gap         = 5
topbar      = 5
border      = 0
prompt      = 60
status      = 20

myNormalBorderColor     = "#000000"
myFocusedBorderColor    = active

active      = orange
activeWarn  = red
inactive    = base02
focusColor  = blue
unfocusColor = base02

myFont      = "-*-terminus-medium-*-*-*-*-160-*-*-*-*-*-*"
myBigFont   = "-*-terminus-medium-*-*-*-*-240-*-*-*-*-*-*"
myWideFont  = "xft:Eurostar Black Extended:"
            ++ "style=Regular:pixelsize=180:hinting=true"
myPromptFont = "xft:Monospace-Bold:pixelsize=24"
myTabFont = "xft:Monospace-Bold:pixelsize=18"

myTabTheme = def
    { fontName            = myTabFont
    , activeColor         = active
    , inactiveColor       = base02
    , activeBorderColor   = active
    , inactiveBorderColor = base02
    , activeTextColor     = base03
    , inactiveTextColor   = base00
    , decoHeight          = 32
    }

-- this is a "fake title" used as a highlight bar in lieu of full borders
-- (I find this a cleaner and less visually intrusive solution)
topBarTheme = def
    { fontName              = myFont
    , inactiveBorderColor   = base03
    , inactiveColor         = base03
    , inactiveTextColor     = base03
    , activeBorderColor     = active
    , activeColor           = active
    , activeTextColor       = active
    , urgentBorderColor     = red
    , urgentTextColor       = yellow
    , decoHeight            = topbar
    }

myPromptTheme = def
    { font                  = myPromptFont
    , bgColor               = base03
    , fgColor               = blue
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

-- Browsers
firefox = "firefox -p default"
chrome = "google-chrome  --profile-directory=\"Default\""
-- work1Chrome = "google-chrome --profile-directory=\"Profile 2\""
-- work2Chrome = "google-chrome --profile-directory=\"Profile 3\""
chromeIncognito = "google-chrome --incognito"
chromium = "chromium"


commands :: X [(String, X ())]
commands = do
  defcmds <- defaultCommands
  return $ defcmds ++ [("meh", mehCmd)
                      , ("clear-clipboard", clearClipboardCmd)
                      , ("dnd-on", dunstDndOn)
                      , ("dnd-off", dunstDndOff)
                      ]

toggleCopyToAll = wsContainingCopies >>= \ws -> case ws of
                      [] -> windows copyToAll
                      _  -> killAllOtherCopies


wsKeys = map show $ [1..9] ++ [0]

myKeys x = let
  subKeys str ks = subtitle str : mkNamedKeymap x ks
  screenKeys     = ["w","e","r"]
  dirKeys        = ["j","k","h","l"]
  arrowKeys        = ["<D>","<U>","<L>","<R>"]
  dirs           = [ D,  U,  L,  R ]
  zipM  m nm ks as f = zipWith (\k d -> (m ++ k, addName nm $ f d)) ks as
  zipM' m nm ks as f b = zipWith (\k d -> (m ++ k, addName nm $ f d b)) ks as
  in
  subKeys "My keys"
           ([ ("M-S-<Return>", addName "Swap master window" $ windows W.swapMaster)
           , ("M-q", addName "Restart XMonad"$ spawn "xmonad --restart")
           , ("M-S-q", addName "Quit XMonad" $ confirmPrompt hotPromptTheme "exit" $ io exitSuccess)
           , ("M-<Return>", addName "Launch Terminal" $ spawn $ XMonad.terminal x) -- %! Launch terminal
           -- , ((myModMask, xK_p), spawn "dmenu_run -fn -*-Monospace-*-r-*-*-16-*-*-*-*-*-*-*")
           -- , ((myModMask, xK_p), shellPrompt myPromptTheme)
           , ("M-p", addName "Launch Rofi" $ spawn "rofi -show run")
           , ("M-<Right>", addName "Next workspace" $ nextWS)
           , ("M-<Home>", addName "Toggle workspace" $ toggleWS' ["NSP"])
           , ("M-<Left>", addName "Previous workspace" $ prevWS)
           , ("M-<Backspace>", addName "Kill window" $ kill)
           , ("M-S-<Backspace>", addName "Killall windows on workspace" $ confirmPrompt hotPromptTheme "kill all" $ killAll)
           , ("M-n", addName "Hide window ?????" $ withFocused hideWindow)
           , ("M-o", addName "Switch project" $ switchProjectPrompt warmPromptTheme)
           , ("M-S-a", addName "Pin window to all workspaces" $ toggleCopyToAll)
           , ("M-S-n", addName "Pop oldest hidden window" $ popOldestHiddenWindow)
           , ("M-C-y", addName "Launch commands menu" $ commands >>= runCommand)
           -- Window navigation and sublayouts
           , ("M-M1-h", addName "???" $ sendMessage $ pullGroup L)
           , ("M-M1-l", addName "???" $ sendMessage $ pullGroup R)
           , ("M-M1-k", addName "???" $ sendMessage $ pullGroup U)
           , ("M-M1-j", addName "???" $ sendMessage $ pullGroup D)
           , ("M-M1-m", addName "Merge all into tabs" $ withFocused (sendMessage . MergeAll))
           , ("M-M1-u", addName "Unmerge all tabs" $ withFocused (sendMessage . UnMerge))
           , ("M-;", addName "Tab down" $ onGroup W.focusUp')
           , ("M-'", addName "Tab up" $ onGroup W.focusDown')
           , ("M-j", addName "Focus down" $ focusDown)
           , ("M-k", addName "Focus up" $ focusUp)
           , ("M-m", addName "Focus master" $ focusMaster)
           -- Scratchpads
           , ("M-C-h", addName "Launch HTOP" $ namedScratchpadAction scratchpads "htop")
           , ("M-C-]", addName "Launch Weechat" $ namedScratchpadAction scratchpads "weechat")
           , ("M-C-g", addName "Launch Google Music" $ namedScratchpadAction scratchpads "googleMusic")
           , ("M-C-[", addName "Launch Signal" $ namedScratchpadAction scratchpads "signal")
           , ("M-C-t", addName "Launch Trello" $ namedScratchpadAction scratchpads "trello")
           , ("M-C-p", addName "Launch keepassX" $ namedScratchpadAction scratchpads "keepassX")
           , ("M-C-o", addName "Launch YubiOAuth" $ namedScratchpadAction scratchpads "yubioath")
           -- , ((myModMask .|. controlMask, xK_bracketright), namedScratchpadAction scratchpads "pidgin")
           -- Applications
           , ("M-<Print>", addName "Print Screen" $ spawn "gnome-screenshot")
           , ("M-S-<Print>", addName "Interactive Print screen" $ spawn "gnome-screenshot --interactive")
           , ("M-S-l", addName "Lock screen" $ spawn "xscreensaver-command -lock")
           , ("C-M1-h", addName "Launch clipboard manager" $ spawn "rofi -modi 'clipboard:greenclip print' -show clipboard -run-command '{cmd}'")
           , ("M-\\", addName "Launch Google Chrome" $ spawn chrome)
           , ("M-i", addName "Launch Google Chrome Icognito" $ spawn chromeIncognito)
           , ("M-/", addName "Launch Firefox" $ spawn firefox)
           -- gotoMenu is not really needed since we have rofi
           , ("M-S-g", addName "DEPRECATED: goto window" $ gotoMenu)
           , ("M-S-b", addName "Bring window" $ bringMenu)
           -- Monitors
           , ("M-C-<F11>", addName "Laptop monitor" $ spawn "/home/deni/dotfiles/scripts/monitors_laptop.sh")
           , ("M-C-<F12>", addName "Work monitor" $ spawn "/home/deni/dotfiles/scripts/monitors_work.sh")
           ]
           -- ++ zipM "M-"                "View ws"                          wsKeys [0..] (withNthWorkspace W.greedyView)
           -- ++ zipM "M-S-"              "Move w to ws"                          wsKeys [0..] (withNthWorkspace W.shift)
           -- ++ zipM  "M-C-"             "Merge w/sublayout"                         dirKeys dirs (sendMessage . pullGroup)
           -- ++ zipM' "M-"               "Navigate screen"                           arrowKeys dirs screenGo True
           -- ++ zipM' "M-C-"             "Move window to screen"                     arrowKeys dirs windowToScreen True
           -- ++ zipM' "M-S-"             "Swap workspace to screen"                  arrowKeys dirs screenSwap True
           )
           ^++^
   subKeys "Media Keys"
           [ ("<XF86AudioMute>", addName "Toggle Sounds" $ toggleMute >>= showAudioMuteAlert)
           , ("<XF86AudioLowerVolume>",  addName "Lower volume" $ lowerVolume 4 >>= alert)
           , ("<XF86AudioRaiseVolume>",  addName "Raise volume" $ raiseVolume 4 >>= alert)
           , ("<XF86AudioMicMute>", addName "Toggle Mic" $ spawn micToggleCmd)
           -- , ("<XF86AudioMicMute>",      toggleMuteMic >>= showMicMuteAlert)
           , ("<XF86MonBrightnessUp>", addName "Increase brightness" $ spawn "/home/deni/dotfiles/scripts/brightness.sh +10")
           , ("<XF86MonBrightnessDown>", addName "Decrease brightness" $ spawn "/home/deni/dotfiles/scripts/brightness.sh -10")
           ]

-- combine with existing keys to we don't have reimplement
-- newKeys x = defaultConfig `additionalKeysP` (myKeys x)
-- newKeys x = (map (fmap noName) $ keys changedKeys)  ^++^ (myKeys x)
--     where changedKeys = removeKeysP defaultConfig []
--                           [ (mod4Mask .|. shiftMask, xK_Return)
--                           , (mod4Mask, xK_Return)
--                           , (mod4Mask .|. shiftMask,   xK_q)
--                           , (mod4Mask .|. shiftMask,   xK_c)
--                           , (mod4Mask .|. shiftMask,   xK_p)
--                           , (mod4Mask, xK_p)
--                           ]



getOffset :: X (Int, Int)
getOffset = withWindowSet $ \W.StackSet {current=W.Screen
  {screenDetail=SD {screenRect=Rectangle {rect_x=x, rect_y=y}}}} -> return $
  (fromIntegral x, fromIntegral y)

getScreens :: IO [ScreenId]
getScreens = do
  screens <- do
    dpy <- openDisplay ""
    rects <- getScreenInfo dpy
    closeDisplay dpy
    return rects
  let ids = zip [0 .. ] screens
  return $ map fst ids

clearClipboardCmd :: X ()
clearClipboardCmd = do
  spawn $ "pkill greenclip && greenclip clear && greenclip daemon &"

dunstDndOn :: X ()
dunstDndOn = do
  spawn $ "notify-send \"DUNST_COMMAND_PAUSE\""

dunstDndOff :: X ()
dunstDndOff = do
  spawn $ "notify-send \"DUNST_COMMAND_RESUME\""
  spawn $ "notify-send \"Do not Disturb\" \"Notifications resumed\""

mehCmd :: X ()
mehCmd = do
  (xOffset, yOffset) <- getOffset
  screens <- liftIO getScreens
  -- Because of Xinerama
  let x = xOffset
  -- 720 == 1440/2
  -- Still need to test with 3 monitors. Will likely need to adjust the x axis as well
  let y = if (yOffset /= 0 && length screens > 1) then 720 else if (length screens > 1) then -720 else yOffset
  spawn $ "echo 'xO:" <> (show $ xOffset) <> " yO:" <> (show $ yOffset) <> "   x:" <> (show x) <> " y:" <> (show y) <> "' >/tmp/dinamo.txt"
  spawn $ "/home/deni/.local/bin/meh" <> " -x " <> (show x) <> " -y " <> (show y)

micToggleCmd = "amixer -q set Capture toggle && amixer get Capture | grep '\\[off\\]' && notify-send \"MIC OFF\" || notify-send \"MIC ON\""

-- NOTE: This was hack when `lowerVolume` and `raiseVolume` were not really working
-- after some alsa/pulseaudio upgrade. Keeping just in case
-- lowerVolumeHack = spawn "amixer -D pulse sset Master 5%-"
-- raiseVolumeHack = spawn "amixer -D pulse sset Master 5%+"

micChannels :: [String]
micChannels = ["Capture"]

toggleMuteMic :: MonadIO m => m Bool
toggleMuteMic = toggleMuteChannels micChannels

alert = D.dzenConfig (centered 150) . show . round
centered w =
        D.onCurr (D.center w 66)
    >=> D.font "-*-helvetica-*-r-*-*-64-*-*-*-*-*-*-*"
    >=> D.addArgs ["-fg", lightgray]
    >=> D.addArgs ["-bg", black]

centeredLarge w =
        D.onCurr (D.center w 450)
    >=> D.font "-*-helvetica-*-r-*-*-224-*-*-*-*-*-*-*"
    >=> D.addArgs ["-fg", lightgray]
    >=> D.addArgs ["-bg", black]

showAudioMuteAlert True  = D.dzenConfig (centered 300) $ "Sound On"
showAudioMuteAlert False = D.dzenConfig (centered 300) $ "Sound Off"

showMicMuteAlert True  = D.dzenConfig (centered 300) $ "Mic on"
showMicMuteAlert False = D.dzenConfig (centered 300) $ "Mic off"

-- COMMANDS
weechatCommand = "urxvt -title WeeChat -e weechat"
isWeechat = (resource =? "urxvt") <&&> (fmap (isInfixOf "WeeChat") title)

htopCommand = "urxvt -title htop -e htop"
isHtop = (title =? "htop")

keepassXCommand = "keepassxc"
keepassXResource =  "keepassxc"
isKeepassX = (resource =? keepassXResource)

-- pidginCommand = "pidgin"
-- pidginResource =  "Pidgin"
-- isPidgin = (appName =? pidginResource)

trelloCommand = "dex $HOME/Desktop/trello.desktop"
trelloResource = "crx_jijnmpkkfkjaihbhffejemnpbbglahim"
isTrello = (resource =? trelloResource)

yubioathCommand = "yubioath-gui"
yubioathResource = "yubioath-gui"
isYubioath = (resource =? yubioathResource)

googleMusicCommand = "dex $HOME/Desktop/googlemusic.desktop"
googleMusicResource = "crx_ioljlgoncmlkbcepmminebblkddfjofl"
isGoogleMusic = (resource =? googleMusicResource)

signalCommand = "signal-desktop"
isSignal = (className =? "Signal")

myStartupHook = do
    spawn "xmodmap ~/.Xmodmap"
    spawn "xrdb -merge -I$HOME ~/.Xresources"

forceCenterFloat :: ManageHook
forceCenterFloat = doFloatDep move
  where
    move :: W.RationalRect -> W.RationalRect
    move _ = W.RationalRect x y w h

    w, h, x, y :: Rational
    w = 1/3
    h = 1/2
    x = (1-w)/2
    y = (1-h)/2

myManageHook = manageHook gnomeConfig
    <+> manageSpecific
    <+> manageDocks
    <+> namedScratchpadManageHook scratchpads
    <+> fullscreenManageHook
    <+> manageSpawn

  where manageSpecific = composeOne
          [ resource  =? "diodon"           -?> doIgnore
          , resource  =? "clipit"           -?> doIgnore
          , resource  =? "parcellite"       -?> doIgnore
          , className =? "Gimp"             -?> doFloat
          -- , className =? "thunderbird"      -?> doFloat
          , className =? "Eog"              -?> doCenterFloat
          , className =? "libreoffice-startcenter" -?> doFullFloat
          , className =? "Zathura"          -?> doCenterFloat
          , className =? "Evince"          -?> doCenterFloat
          , className =? "vlc"              -?> doCenterFloat
          , className =? "VirtualBox Manager" -?> doCenterFloat
          , className =? "Signal"           -?> doCenterFloat
          , className =? "zoom"             -?> doCenterFloat
          , className =? "Gnome-calculator" -?> doCenterFloat
          , className =? "Keybase"          -?> doCenterFloat
          , className =? "Totem"            -?> doCenterFloat
          , className =? "Keepassx"         -?> doCenterFloat
          , className =? "SpiderOakONE"     -?> doCenterFloat
          , isHtop                          -?> (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
          , className =? "Gnome-Screenshot" -?> doIgnore
          , className =? "Pidgin"           -?> doShift "2"
          , className =? "skypeforlinux"    -?> doShift "2"
          , classNotRole ("Pidgin", "buddy_list") -?> doShift "2"
          , transience
          , isRole =? gtkFile  -?> forceCenterFloat
          , isChromeDialog -?> forceCenterFloat
          , isFirefoxDialog -?> forceCenterFloat
          , isRole =? "pop-up" -?> doCenterFloat
          , isDialog -?> doCenterFloat
          , isInProperty "_NET_WM_WINDOW_TYPE"
                         "_NET_WM_WINDOW_TYPE_SPLASH" -?> doCenterFloat
          , resource =? "console" -?> tileBelowNoFocus
          , isFullscreen -?> doFullFloat
          , pure True -?> tileBelow
          ]
        tileBelow = insertPosition Below Newer
        tileBelowNoFocus = insertPosition Below Older
        isRole = stringProperty "WM_WINDOW_ROLE"
        gtkFile = "GtkFileChooserDialog"
        classNotRole (c,r) = (className =? c) <&&> (role /=? r)
        isChromeDialog = isDialog <&&> className =? "google-chrome"
        isFirefoxDialog = isDialog <&&> className =? "Firefox"
        role = stringProperty "WM_WINDOW_ROLE"


scratchpads = [
    -- run htop in xterm, find it by title, use default floating window placement
    NS "weechat" weechatCommand isWeechat (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
  , NS "htop" htopCommand isHtop (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
  , NS "signal" signalCommand isSignal doCenterFloat
  , NS "googleMusic" googleMusicCommand isGoogleMusic doCenterFloat
  , NS "keepassX" keepassXCommand isKeepassX doCenterFloat
  , NS "trello" trelloCommand isTrello doCenterFloat
  , NS "yubioath" yubioathCommand isYubioath doCenterFloat
  -- , NS "pidgin" pidginCommand isPidgin defaultFloating
  ]

-- dontShrinkText :: CustomShrink
-- dontShrinkText = CustomShrink

-- instance Shrinker CustomShrink where
--     shrinkIt _ "" = [""]
--     shrinkIt s cs = [cs]

data FULLBAR = FULLBAR deriving (Read, Show, Eq, Typeable)
instance Transformer FULLBAR Window where
    transform FULLBAR x k = k barFull (\_ -> x)

-- tabBarFull = avoidStruts $ noFrillsDeco shrinkText topBarTheme $ addTabs shrinkText myTabTheme $ Simplest
barFull = avoidStruts $ Simplest

-- Numbered
-- 1 is Firefox (personal)
-- 2 is Chrome (work) (TODO: Move IM layout from here)
wsSYS1 = "3" -- random stuff (one terminal)
wsSYS2 = "4" -- more random sys stuff (three terminals)
wsPROJ = "5" -- working on projects (3 terminals)
-- 6 is freeform (open random apps here)
-- 7 is freeform (open random apps here)
wsWORK1 = "8" -- Work (3 terminals by default)
wsWORK2 = "9" -- Work (2 terminals by default)

-- Named
wsHS = "hs" -- haskell
wsPURS = "purs" -- purescript
wsEMAIL = "email" -- email
wsDMO = "dmo"  -- random demo workspace

-- Projects
projects :: [Project]
projects = [ Project { projectName  = wsDMO
                     , projectDirectory  = "~/"
                     , projectStartHook  = Just $ do spawn "/usr/lib/xscreensaver/spheremonics"
                                                     runInTerm "-title top" "top"
                                                     runInTerm "-title htop" "htop"
                                                     spawn "/usr/lib/xscreensaver/cubicgrid"
                                                     spawn "/usr/lib/xscreensaver/surfaces"
                     }
           , Project { projectName  = wsSYS1
                     , projectDirectory  = "~/"
                     , projectStartHook  = Just $ spawnOn wsSYS1 myTerminal
                     }
           , Project { projectName  = wsSYS2
                     , projectDirectory  = "~/"
                     , projectStartHook  = Just $ do spawnOn wsSYS2 myTerminal
                                                     spawnOn wsSYS2 myTerminal
                                                     spawnOn wsSYS2 myTerminal
                     }
           , Project { projectName  = wsPROJ
                     , projectDirectory  = "~/projects"
                     , projectStartHook  = Just $ do spawnOn wsPROJ myTerminal
                                                     spawnOn wsPROJ myTerminal
                                                     spawnOn wsPROJ myTerminal
                     }
           , Project { projectName  = wsWORK1
                     , projectDirectory  = "~/work"
                     , projectStartHook  = Just $ do spawnOn wsWORK1 myTerminal
                                                     spawnOn wsWORK1 myTerminal
                                                     spawnOn wsWORK2 myTerminal
                     }
           , Project { projectName  = wsWORK2
                     , projectDirectory  = "~/work"
                     , projectStartHook  = Just $ do spawnOn wsWORK2 myTerminal
                                                     spawnOn wsWORK2 myTerminal
                     }
           , Project { projectName  = wsPURS
                     , projectDirectory  = "~/"
                     , projectStartHook  = Just $ do spawnOn wsPURS myTerminal
                                                     spawnOn wsPURS myTerminal
                     }
           , Project { projectName  = wsHS
                     , projectDirectory  = "~/"
                     , projectStartHook  = Just $ do spawnOn wsHS myTerminal
                                                     spawnOn wsHS myTerminal
                     }
           , Project { projectName  = wsEMAIL
                     , projectDirectory  = "~/"
                     , projectStartHook  = Just $ do spawnOn wsEMAIL "thunderbird"
                     }
           ]

-- This is the event hook from XMonad.Layout.Fullscreen which will constrain the full
-- screen into the window frame rathe actually doing the "overlay" full screen.
-- In pracice this means I can watch a video in "fullscreen" on one part of the screen
-- while having a window open in another part. Useful for lectures.
-- I can always full screen the window frame with the video obviously.
-- The downiside is that xmobar is visible. But I like that.
-- If true full screen is desired use fullscreenEventHook from XMonad.Hooks.EwmhDesktops
myHandleEventHook = docksEventHook
                <+> fadeWindowsEventHook
                <+> handleEventHook def
                <+> fullscreenEventHook

-- Layouts
myLayout = renamed [ CutWordsLeft 4 ]
     $ smartBorders
     $ noFrillsDeco shrinkText topBarTheme
     $ spacing gap
     $ fullscreenFloat -- fixes floating windows going full screen, while retaining "bounded" fullscreen
     $ mkToggle (single FULLBAR)
     $ windowNavigation
     $ boringWindows
     $ avoidStruts
     $ addTabsBottom shrinkText myTabTheme
     $ subLayout [] (Simplest ||| Accordion)
     $ onWorkspace "2" imLayout $ myLayouts
  where
    --          numMasters, resizeIncr, splitRatio
    tall = Tall 1           0.02        0.5
    -- define the list of standardLayouts
    myLayouts = (layoutHook defaultConfig) ||| Accordion ||| Grid ||| emptyBSP
    -- notice that withIM, which normally acts on one layout, can also
    -- work on a list of layouts (yay recursive data types!)
    imLayout = withIM (2/10) (Role "buddy_list") myLayouts

myFadeHook = composeAll
    [ opaque -- default to opaque
    , isUnfocused --> opacity 0.85
    , (className =? "URxvt") <&&> (isUnfocused) --> opacity 0.9
    , fmap ("Google" `isPrefixOf`) className --> opaque
    , isDialog --> opaque
    --, isUnfocused --> opacity 0.55
    --, isFloating  --> opacity 0.75
    ]

-- LogHook
myLogHook h = do
    -- following block for copy windows marking
    copies <- wsContainingCopies
    let check ws | ws `elem` copies =
                   pad . xmobarColor yellow red . wrap "*" " "  $ ws
                 | otherwise = pad ws
    fadeWindowsLogHook myFadeHook
    ewmhDesktopsLogHook
    dynamicLogWithPP xmobarPP
      { ppOutput = hPutStrLn h
      , ppTitle  = xmobarColor "green" "" . shorten 50
      }

myModMask = mod4Mask
altMask = mod1Mask

-- Display keyboard mappings using zenity
-- from https://github.com/thomasf/dotfiles-thomasf-xmonad/
--              blob/master/.xmonad/lib/XMonad/Config/A00001.hs
showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
    h <- spawnPipe "zenity --font=terminus --text-info --width=1500 --height=1000"
    hPutStr h (unlines $ showKm x)
    hClose h
    return ()

---------------------------------------------------------------------------
-- Urgency Hook
---------------------------------------------------------------------------
-- from https://pbrisbin.com/posts/using_notify_osd_for_xmonad_notifications/
data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

-- https://github.com/pjones/xmonadrc
instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name     <- getName w
        Just idx <- fmap (W.findTag w) $ gets windowset

        safeSpawn "notify-send" [show name, "workspace " ++ idx]

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar /home/deni/.xmobarrc"
    xmonad
      $ withUrgencyHook LibNotifyUrgencyHook
      $ ewmh
      -- We should use addDescrKeys' to *not* merge the default keys but then
      -- we have to define them all for ourselves.
      -- For now I'm okay living with slightly wrong help strings so I don't have
      -- to redefine the basic key bindings like movement and resizing
      $ addDescrKeys ((myModMask .|. shiftMask, xK_slash), showKeybindings) myKeys
      $ dynamicProjects projects
      $ gnomeConfig
        { manageHook          = myManageHook
        , layoutHook          = myLayout
        , logHook             = myLogHook xmproc
        , terminal            = myTerminal
        , borderWidth         = border
        , startupHook         = myStartupHook
        , handleEventHook     = myHandleEventHook
        , modMask             = mod4Mask -- super key
        , focusFollowsMouse   = True
        }
