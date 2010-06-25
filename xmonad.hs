{-# LANGUAGE FlexibleContexts #-}
{- xmonad.hs
 - Author: Jurica Bradaric 
 - Version: 0.9.1
 -}

-------------------------------------------------------------------------------
-- Imports --
{- misc -}
--import Data.List(isInfixOf)
import Graphics.X11.Xlib
import IO (Handle, hPutStrLn) 
import Control.Monad(when)

-- XMonad stuff
import XMonad
import qualified XMonad.StackSet as W

-- utils
import XMonad.Util.Run (spawnPipe,runInTerm)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Themes (theme,donaldTheme)
import XMonad.Util.Scratchpad (scratchpadSpawnAction)
import XMonad.Util.XSelection (getSelection)
import qualified XMonad.Util.Loggers as Logger

-- hooks
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.UrgencyHook

-- layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.IM
import XMonad.Layout.Grid
import XMonad.Layout.PerWorkspace
import XMonad.Layout.LayoutHints
import XMonad.Layout.Tabbed
import XMonad.Layout.Circle
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Accordion
import Data.Ratio((%))

-- prompts
import XMonad.Prompt
import XMonad.Prompt.AppendFile
import XMonad.Prompt.Input

-- actions
import XMonad.Actions.WindowGo (runOrRaise, raiseMaybe)
import XMonad.Actions.FocusNth (focusNth)
import qualified XMonad.Actions.Search as S

-- extra keys
import Graphics.X11.ExtraTypes.XF86

import XMonad.Util.Run(safeSpawn)
import qualified Data.Map as M

import Config

runOrRaise' :: FilePath -> [String] -> Query Bool -> X ()
runOrRaise' fp args query = raiseMaybe (safeSpawn fp args) query
--runOrRaise' = raiseMaybe . safeSpawn

-------------------------------------------------------------------------------
-- Main --
main = do
       --h <- spawnPipe "xmobar" 
       h <- spawnPipe myStatusBar
       --conky <- spawnPipe myConky
       xmonad $ withUrgencyHook dzenUrgencyHook { args = ["-bg", "darkgreen", "-xs", "1"] } $ myConfig
        { logHook = logHook' h } `additionalKeysP` myKeys

{-
 - myStatusBar :: String
 - myStatusBar = "dzen2 -e 'entertitle:uncollapse;leavetitle:collapse' -x '0' -h '16' -w '300' -ta l -fg '" ++ myNormalFGColor ++ "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "'"
 - --myConky = "conky -c ~/.myconk | dzen2  -e '' -x '300' -h '12' -w '880' -ta r -fg '#ccccdd' -bg '#000000' -fn '" ++ myFont ++ "'"
 - myFont = "Monaco-8"
 - --myFont = "-*-terminus-*-*-*-*-10-*-*-*-*-*-*-*"
 - myDzenFGColor = "#ccccdd"
 - myDzenBGColor = "#000000"
 - myNormalFGColor = "#ccccdd"
 - myNormalBGColor = "#000000"
 - myFocusedFGColor = "#0000ff"
 - myFocusedBGColor = "#000000"
 - myUrgentFGColor = "#0099ff"
 - myUrgentBGColor = "#000000"
 - myIconFGColor = "#777777"
 - myIconBGColor = "#000000"
 - mySeperatorColor = "#555555"
 -}

-------------------------------------------------------------------------------

{-
 - myConfig = defaultConfig
 -     { workspaces = workspaces'
 -     , modMask = modMask'
 -     , borderWidth = borderWidth'
 -     , normalBorderColor = normalBorderColor'
 -     , focusedBorderColor = focusedBorderColor'
 -     , terminal = terminal'
 -     , layoutHook = layoutHook'
 -     , manageHook = manageHook'
 -     }
 -}

-- Hooks --
{-
 - myManageHook = (composeAll . concat)
 -     [ [className =? name --> doShift wspace | (name, wspace) <- myShifts]
 -     , [className =? name --> doFloat | name <- myFloats]
 -     , [className =? "Namoroka" <&&> stringProperty "WM_WINDOW_ROLE" =? "Manager" --> doShift "mail" <+> doF W.focusDown]
 -     , [className =? "Namoroka" <&&> stringProperty "WM_WINDOW_ROLE" =? "browser" --> doShift "web"  <+> doF W.focusDown]
 -     , [className =? "Firefox" <&&> stringProperty "WM_WINDOW_ROLE" =? "Manager" --> doShift "mail" <+> doF W.focusDown]
 -     , [className =? "Firefox" <&&> stringProperty "WM_WINDOW_ROLE" =? "browser" --> doShift "web"  <+> doF W.focusDown]
 -     , [resource  =? resName --> doIgnore | resName <- myIgnored]
 -     , [className =? name --> doShift wspace <+> doF W.focusDown | (name, wspace) <- myBackgrounded]
 -     , [className =? "stalonetray" --> doIgnore]
 -     , [stringProperty "WM_NAME" =? "ncmpcpp" --> doFloat]
 -     ]
 -     where myShifts = [ ("Pidgin", "im")
 -                      , ("MPlayer", "media")
 -                      , ("Smplayer", "media")
 -                      , ("Gvim", "dev")
 -                      , ("Evince", "reading")
 -                      , ("Acroread", "reading")
 -                      , ("IRSSI", "irc")
 -                      ]
 -           myFloats = ["Gimp"]
 -           myIgnored = ["desktop_window", "adl"]
 -           myBackgrounded = [("uzbl", "web")]
 -}

{-
 - manageHook' :: ManageHook
 - --manageHook' = doF W.swapDown <+> manageHook defaultConfig <+> manageDocks <+> myManageHook
 - manageHook' = foldr1 (<+>)
 -             [ doF W.swapDown
 -             , manageHook defaultConfig
 -             , manageDocks
 -             , myManageHook
 -             ]
 -}

isFloat :: Query Bool
isFloat = ask >>= (\w -> liftX $ withWindowSet $ \ws -> return $ M.member w $ W.floating ws)

-- | Set the border color when the query is satisfied.  Should be added to the
--   ManageHook.
colorWhen :: Query Bool -> String -> X ()
colorWhen q cl = withFocused $ \w -> runQuery q w >>= flip when (setWindowBorder' cl w)

-- | Give set the border color of a window to the given HTML color code.
setWindowBorder' ::(MonadReader XConf m, MonadIO m) => String -> Window -> m ()
setWindowBorder' c w = do
    XConf { display = d } <- ask
    ~(Just pc) <- io $ initColor d c
    io $ setWindowBorder d w pc


logHook' :: Handle ->  X ()
logHook' h = do
    dynamicLogWithPP (customPP { ppOutput = hPutStrLn h })
    fadeInactiveLogHook fadeAmount
    colorWhen isFloat focusedBorderColor'

{-
 - fadeAmount :: Rational
 - fadeAmount = 0.4
 -}

{- layoutHook' = customLayout -}

-------------------------------------------------------------------------------
{-
-- Looks --
-}
-- status bar
customPP = dzenPP 
    { ppCurrent = dzenColor "#0b8bff" "#000000"
    , ppUrgent = wrap "^fg(#ff0000)[" "]^fg()" . trim . dzenStrip -- . dzenColor "#ff0000" "#000000"
    , ppSep = "^fg(#333333) | ^fg()"
    , ppTitle = const ""
    , ppHiddenNoWindows = const ""
    {- Do not show the NSP workspace -}
    , ppHidden = \s -> if s == "NSP" then "" else wrap " ^bg()^fg(#333333)" " ^fg()" s
    , ppLayout = wrap " ^bg()^fg(#222222)" " ^fg()"
    }

-- borders
{-
 - borderWidth' :: Dimension
 - borderWidth' = 1
 - 
 - normalBorderColor', focusedBorderColor' :: String
 - normalBorderColor'  = "#333333"
 - focusedBorderColor' = "#0775a8"
 - 
 - -- workspaces
 - workspaces' :: [WorkspaceId]
 - workspaces' = ["term", "dev", "web", "media", "irc", "im", "reading", "vm", "mail"]
 -}

-- layouts
{-
 - customLayout = avoidStruts 
 -                 $ onWorkspaces ["dev", "web"] (noBorders (tabbed shrinkText (theme donaldTheme)) ||| smartBorders threeCols)
 -                 $ onWorkspace "irc" (noBorders Circle ||| smartBorders threeCols ||| noBorders Full ||| noBorders Accordion)
 -                 $ onWorkspace "term" (smartBorders threeCols ||| smartBorders Full ||| smartBorders Accordion)
 -                 $ onWorkspace "im" (noBorders threeCols ||| Grid)
 -                 $ onWorkspace "reading" (smartBorders (tabbed shrinkText (theme donaldTheme)))
 -                 $ onWorkspace "media" (smartBorders Circle ||| smartBorders Full)
 -                 $ onWorkspace "vm" (noBorders Full ||| smartBorders threeCols)
 -                 $ onWorkspace "mail" (noBorders Full ||| smartBorders threeCols)
 -                 $ smartBorders tiled 
 -     where
 -     tiled = ResizableTall 1 (2/100) (1/2) []
 -     threeCols = ThreeCol 1 (3/100) (1/2)
 -}

-------------------------------------------------------------------------------
-- Terminal --
{-
 - terminal' :: String
 - terminal' = "urxvtc"
 -}

-------------------------------------------------------------------------------
-- Keys/Button bindings --
-- modmask
{-
 - modMask' :: KeyMask
 - modMask' = mod4Mask
 - 
 - myDarkXPC :: XPConfig
 - myDarkXPC = defaultXPConfig
 -     { font              = "xft:Monaco 9"
 -     , height            = 28
 -     , bgColor           = "black"
 -     , fgColor           = "#684"
 -     , bgHLight          = "#785"
 -     , fgHLight          = "black"
 -     , promptBorderWidth = 0
 -     , position          = Top
 -     }
 -}

myScratchpadConf = defaultConfig
    { terminal = "urxvtc"
    }

myBrowser :: String
myBrowser = "firefox"

myKeys :: [(String, X ())]
myKeys = concat
        [
         [ ("M-f", windows (W.greedyView "web") >> runOrRaise "firefox" (className =? "Namoroka"))
         {- [ ("M-f", windows (W.greedyView "web") >> runOrRaise "firefox" (className =? "Firefox")) -}
         , ("M-p", windows (W.greedyView "im") >> spawn "pidgin")
         , ("M-a", windows (W.greedyView "media") >> spawn "sonata")
         , ("M-i", windows (W.greedyView "irc") >> spawn "urxvtc -title IRSSI -e irssi")
         , ("M-<Left>", spawn "mpc prev")
         , ("M-<Right>", spawn "mpc next")
         , ("M-<Up>", spawn "mpc toggle")
         , ("<XF86AudioLowerVolume>", spawn "amixer -q sset Master 1-")
         , ("<XF86AudioRaiseVolume>", spawn "amixer -q sset Master 1+")
         , ("<XF86AudioMute>", spawn "amixer -q sset Master toggle")
         , ("<XF86ScreenSaver>", spawn "slock")
         , ("M-l", spawn "slock")
         , ("M-n", appendFilePrompt myDarkXPC "/home/m00nblade/NOTES")
         , ("M-S-x", sendMessage ToggleStruts)
         , ("M-S-t", scratchpadSpawnAction myScratchpadConf)
         , ("M-d d", getSelection >>= spawnDictionary myDictionary)
         , ("M-d t", getSelection >>= spawnDictionary myThesaurus)
         , ("M-d m", inputPrompt myDarkXPC "Dictionary search" >>= spawnDictionary' myDictionary)
         , ("M-c s", spawn "/home/m00nblade/.scripts/switch_conk.sh") -- switch conky configuration to match the running network interface
         , ("M-w s", spawn "/home/m00nblade/.scripts/wallpaper.sh") -- switch to a random wallpaper
         , ("M-m", runOrRaise' "/usr/bin/urxvtc" ["-geometry", "120x40+100+50", "-name", "ncmpcpp", "-e", "ncmpcpp"] (stringProperty "WM_NAME" =? "ncmpcpp")) -- XXX
         ]
         {- focus the nth window with <Ctrl>-# -}
         , [ ("C-" ++ show k, focusNth i) | (i, k) <- zip [0..8] [1..]]
        ]

{- dictionary and thesaurus to use for word lookup -}
myDictionary, myThesaurus :: String
myDictionary = " -d wn "
myThesaurus = " -d moby-thesaurus "

 
{-
 - Checks if there was actually an input from the user.
 - If there wasn't, do nothing.
 -}
spawnDictionary' :: String -> Maybe String -> X ()
spawnDictionary' dict word =
    case word of
         Just word -> spawnDictionary dict word
         Nothing -> return ()

{-
 - The arguments to the sed utility. Used to highlight some words in
 - the output of dict.
 -}
sedArgs :: String
sedArgs =  "-e '1,4d' "
        ++ "-e 's/{\\([^}]*\\)}/^fg(white)\\1^fg()/g' "
        ++ "-e 's/\\(\\[syn:\\)/^fg(#ccd)\\1^fg()/g' "
{-
 - Show a dzen2 window with the output of the dict program for
 - the given word
 -}
spawnDictionary :: String -> String -> X ()
spawnDictionary args word = spawn $ "dict " 
                               ++ args ++ " '" ++ word ++ "' "
                               ++ " | sed " ++ sedArgs
                               ++ " | dzen2 -l 16 -p -w 700 "
                               ++ "-bg '" ++ bgColor myDarkXPC ++ "' "
                               ++ "-fg '" ++ fgColor myDarkXPC ++ "' " 
                               ++ "-fn 'Monaco-9' "
                               ++ "-x 300 -y 300 "
                               ++ "-e 'onstart=scrollhome,uncollapse;"
                               ++ "button4=scrollup;"
                               ++ "button5=scrolldown;"
                               ++ "button1=exit'"
