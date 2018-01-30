--
-- Xmonad Configuration by Deni Bertovic
--

import           Control.Monad
import           Data.List                          (isInfixOf)
import qualified Data.Map                           as M
import           Data.Monoid                        ((<>))
import           System.Exit
import           System.IO
import           XMonad
import           XMonad.Actions.Commands
import           XMonad.Actions.CopyWindow          (copyToAll,
                                                     killAllOtherCopies,
                                                     wsContainingCopies)
import           XMonad.Actions.CycleWS
import           XMonad.Actions.DynamicProjects
import           XMonad.Actions.DynamicProjects
import           XMonad.Actions.SpawnOn             (spawnOn)
import           XMonad.Actions.Volume
import           XMonad.Actions.WithAll             (killAll)
import           XMonad.Config.Gnome
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.Accordion            (Accordion (..))
import           XMonad.Layout.BinarySpacePartition
import           XMonad.Layout.BoringWindows
import           XMonad.Layout.Grid
import           XMonad.Layout.Hidden               (hideWindow,
                                                     popOldestHiddenWindow)
import           XMonad.Layout.IM
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Simplest             (Simplest (..))
import           XMonad.Layout.SubLayouts
import           XMonad.Layout.Tabbed               (CustomShrink (..),
                                                     Shrinker (..), Theme (..),
                                                     addTabs, shrinkText,
                                                     simpleTabbed, tabbed)
import           XMonad.Layout.WindowNavigation
import           XMonad.ManageHook
import           XMonad.Prompt                      (XPConfig (..),
                                                     XPPosition (..),
                                                     defaultXPConfig)
import           XMonad.Prompt.ConfirmPrompt
import           XMonad.Prompt.Shell                (shellPrompt)
import qualified XMonad.StackSet                    as W
import           XMonad.Util.Dmenu
import qualified XMonad.Util.Dzen                   as D
import           XMonad.Util.EZConfig               (additionalKeys,
                                                     additionalKeysP,
                                                     removeKeys)
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run                    (runInTerm, spawnPipe)
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
gap         = 10
topbar      = 10
border      = 0
prompt      = 50
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
myPromptFont = "xft:Monospace-Bold:pixelsize=24"
myTabFont = "xft:Monospace-Bold:pixelsize=32"

-- Browsers
firefox = "firefox"
chrome = "google-chrome  --profile-directory=\"Default\""
work1Chrome = "google-chrome --profile-directory=\"Profile 2\""
work2Chrome = "google-chrome --profile-directory=\"Profile 3\""
chromeIncognito = "google-chrome --incognito"
chromium = "chromium"

myTabTheme = def
    { fontName            = myTabFont
    , activeColor         = active
    , inactiveColor       = base02
    , activeBorderColor   = active
    , inactiveBorderColor = base02
    , activeTextColor     = base03
    , inactiveTextColor   = base00
    , decoHeight          = 40
    }

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

toggleCopyToAll = wsContainingCopies >>= \ws -> case ws of
                      [] -> windows copyToAll
                      _  -> killAllOtherCopies


myKeys x = [ ((mod4Mask .|. shiftMask, xK_Return), windows W.swapMaster)
           , ((mod4Mask .|. shiftMask, xK_q), confirmPrompt hotPromptTheme "exit" $ io exitSuccess)
           , ((modMask x, xK_Return), spawn $ XMonad.terminal x) -- %! Launch terminal
           -- , ((mod4Mask, xK_p), spawn "dmenu_run -fn -*-Monospace-*-r-*-*-16-*-*-*-*-*-*-*")
           , ((mod4Mask, xK_p), shellPrompt myPromptTheme)
           , ((modMask x, xK_Right), nextWS)
           , ((modMask x, xK_Right), nextWS)
           , ((modMask x, xK_Home), toggleWS' ["NSP"])
           , ((modMask x, xK_Left), prevWS)
           , ((modMask x, xK_BackSpace), kill)
           , ((mod4Mask .|. shiftMask, xK_BackSpace), confirmPrompt hotPromptTheme "kill all" $ killAll)
           , ((modMask x, xK_n), withFocused hideWindow)
           , ((modMask x, xK_o), switchProjectPrompt warmPromptTheme)
           , ((mod4Mask .|. shiftMask, xK_a), toggleCopyToAll)
           , ((mod4Mask .|. shiftMask, xK_n), popOldestHiddenWindow)
           , ((mod4Mask .|. controlMask, xK_y), commands >>= runCommand)
           -- Window navigation and sublayouts
           , ((mod4Mask .|. controlMask, xK_h), sendMessage $ pullGroup L)
           , ((mod4Mask .|. controlMask, xK_l), sendMessage $ pullGroup R)
           , ((mod4Mask .|. controlMask, xK_k), sendMessage $ pullGroup U)
           , ((mod4Mask .|. controlMask, xK_j), sendMessage $ pullGroup D)
           , ((mod4Mask .|. controlMask, xK_m), withFocused (sendMessage . MergeAll))
           , ((mod4Mask .|. controlMask, xK_u), withFocused (sendMessage . UnMerge))
           , ((mod4Mask .|. controlMask, xK_period), onGroup W.focusUp')
           , ((mod4Mask .|. controlMask, xK_comma), onGroup W.focusDown')
           , ((modMask x, xK_j), focusUp)
           , ((modMask x, xK_k), focusDown)
           , ((modMask x, xK_m), focusMaster)
           -- Scratchpads
           , ((mod4Mask .|. controlMask, xK_h), namedScratchpadAction scratchpads "htop")
           , ((mod4Mask .|. controlMask, xK_bracketleft), namedScratchpadAction scratchpads "weechat")
           , ((mod4Mask .|. controlMask, xK_g), namedScratchpadAction scratchpads "googleMusic")
           , ((mod4Mask .|. controlMask, xK_t), namedScratchpadAction scratchpads "trello")
           , ((mod4Mask .|. controlMask, xK_p), namedScratchpadAction scratchpads "keepassX")
           , ((mod4Mask .|. controlMask, xK_o), namedScratchpadAction scratchpads "yubioath")
           -- , ((mod4Mask .|. controlMask, xK_bracketright), namedScratchpadAction scratchpads "pidgin")
           -- Applications
           , ((modMask x, xK_Print), spawn "gnome-screenshot")
           , ((mod4Mask .|. shiftMask, xK_Print), spawn "gnome-screenshot --interactive")
           , ((mod4Mask .|. shiftMask, xK_l), spawn "xscreensaver-command -lock")
           , ((controlMask .|. mod1Mask, xK_h), spawn "diodon")
           , ((modMask x, xK_backslash), spawn chrome)
           , ((modMask x, xK_i), spawn chromeIncognito)
           , ((modMask x, xK_slash), spawn firefox)
           , ((modMask x, xK_y), spawn "yubioath-gui")
           , ((modMask x, xK_semicolon), spawn work1Chrome)
           , ((modMask x, xK_apostrophe), spawn work2Chrome)
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
    where changedKeys = removeKeys defaultConfig [
                            (mod4Mask .|. shiftMask, xK_Return)
                          , (mod4Mask, xK_Return)
                          , (mod4Mask .|. shiftMask,   xK_q)
                          , (mod4Mask .|. shiftMask,   xK_c)
                          , (mod4Mask .|. shiftMask,   xK_p)
                          , (mod4Mask, xK_p)
                          ]

alert = D.dzenConfig (centered 150) . show . round
centered w =
        D.onCurr (D.center w 66)
    >=> D.font "-*-helvetica-*-r-*-*-64-*-*-*-*-*-*-*"
    >=> D.addArgs ["-fg", lightgray]
    >=> D.addArgs ["-bg", black]

showAudioMuteAlert True  = D.dzenConfig (centered 300) $ "Sound Off"
showAudioMuteAlert False = D.dzenConfig (centered 300) $ "Sound On"

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
trelloResource = "crx_ncbimcinaaoicbdgigfhhkjkohgkdffc"
isTrello = (resource =? trelloResource)

yubioathCommand = "yubioath-gui"
yubioathResource = "yubioath-gui"
isYubioath = (resource =? yubioathResource)

googleMusicCommand = "dex $HOME/Desktop/googlemusic.desktop"
googleMusicResource = "crx_mohcaplidabfmbioljcgponkanhekdbf"
isGoogleMusic = (resource =? googleMusicResource)

myStartupHook = do
    spawn "xmodmap ~/.Xmodmap"
    spawn "xrdb -merge -I$HOME ~/.Xresources"

myManageHook = composeAll
    [ manageHook gnomeConfig
    , resource  =? "diodon"           --> doIgnore
    , className =? "Gimp"             --> doFloat
    , className =? "Gnome-calculator" --> doFloat
    , className =? "Keybase"          --> doFloat
    , className =? "Totem"            --> doFloat
    , className =? "Keepassx"         --> doFloat
    , className =? "SpiderOakONE"     --> doFloat
    , className =? "Gnome-Screenshot" --> doIgnore
    , className =? "Pidgin"           --> doShift "2"
    , className =? "skypeforlinux"    --> doShift "2"
    , classNotRole ("Pidgin", "buddy_list") --> doShift "2"
    , manageDocks
    ]
  where classNotRole :: (String, String) -> Query Bool
        classNotRole (c,r) = (className =? c) <&&> (role /=? r)
        role = stringProperty "WM_WINDOW_ROLE"


scratchpads = [
    -- run htop in xterm, find it by title, use default floating window placement
    NS "weechat" weechatCommand isWeechat (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
  , NS "htop" htopCommand isHtop (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
  , NS "googleMusic" googleMusicCommand isGoogleMusic defaultFloating
  , NS "keepassX" keepassXCommand isKeepassX defaultFloating
  , NS "trello" trelloCommand isTrello defaultFloating
  , NS "yubioath" yubioathCommand isYubioath defaultFloating
  -- , NS "pidgin" pidginCommand isPidgin defaultFloating
  ]

dontShrinkText :: CustomShrink
dontShrinkText = CustomShrink

instance Shrinker CustomShrink where
    shrinkIt _ "" = [""]
    shrinkIt s cs = [cs]

-- Layouts
myLayout = smartBorders
     $ windowNavigation
     $ boringWindows
     $ avoidStruts
     $ addTabs shrinkText myTabTheme
     $ subLayout [] (Simplest ||| Accordion)
     $ onWorkspace "2" imLayout $ myLayouts
  where
    --          numMasters, resizeIncr, splitRatio
    tall = Tall 1           0.02        0.5
    -- define the list of standardLayouts
    myLayouts = (layoutHook defaultConfig) ||| Grid ||| emptyBSP
    -- notice that withIM, which normally acts on one layout, can also
    -- work on a list of layouts (yay recursive data types!)
    imLayout = withIM (2/10) (Role "buddy_list") myLayouts


-- Numbered
-- 1 is Firefox (personal)
-- 2 is IM (Piding/Skype)
wsSYS1 = "3" -- random sys stuff
wsSYS2 = "4" -- more random sys stuff
wsPROJ = "5" -- working on projects
-- 6 is freeform (open random apps here)
-- 7 is Chrome (work)
wsWORK1 = "8"
wsWORK2 = "9"

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
                                                     runInTerm "-title top" "htop"
                                                     spawn "/usr/lib/xscreensaver/cubicgrid"
                                                     spawn "/usr/lib/xscreensaver/surfaces"
                     }
           , Project { projectName  = wsSYS1
                     , projectDirectory  = "~/"
                     , projectStartHook  = Just $ do spawnOn wsSYS1 myTerminal
                                                     spawnOn wsSYS1 myTerminal
                                                     spawnOn wsSYS1 myTerminal
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

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar /home/deni/.xmobarrc"
    xmonad $ dynamicProjects projects $ gnomeConfig
        { manageHook          = myManageHook <> namedScratchpadManageHook scratchpads
        , layoutHook          = myLayout
        , logHook             = dynamicLogWithPP xmobarPP
                              { ppOutput = hPutStrLn xmproc
                              , ppTitle  = xmobarColor "green" "" . shorten 50
                              }
        , terminal            = myTerminal
        , startupHook         = myStartupHook
        , keys                = newKeys
        , modMask             = mod4Mask -- super key
        , focusFollowsMouse   = True
        } `additionalKeysP` specialKeys

