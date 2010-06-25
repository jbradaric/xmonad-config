module Config where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Prompt --(XPConfig, defaultXPConfig)

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

-- utils
import XMonad.Util.Themes (theme,donaldTheme)

myStatusBar = "dzen2 -e 'entertitle:uncollapse;leavetitle:collapse' -x '0' -h '16' -w '300' -ta l -fg '" ++ myNormalFGColor ++ "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "'"
myFont = "Monaco-8"
myDzenFGColor = "#ccccdd"
myDzenBGColor = "#000000"
myNormalFGColor = "#ccccdd"
myNormalBGColor = "#000000"
myFocusedFGColor = "#0000ff"
myFocusedBGColor = "#000000"
myUrgentFGColor = "#0099ff"
myUrgentBGColor = "#000000"
myIconFGColor = "#777777"
myIconBGColor = "#000000"
mySeperatorColor = "#555555"

workspaces' :: [WorkspaceId]
workspaces' = [ "term"
              , "dev"
              , "web"
              , "media"
              , "irc"
              , "im"
              , "reading"
              , "vm"
              , "mail"
              ]

terminal' :: String
terminal' = "urxvtc"

modMask' :: KeyMask
modMask' = mod4Mask

borderWidth' :: Dimension
borderWidth' = 1

normalBorderColor', focusedBorderColor' :: String
normalBorderColor'  = "#333333"
focusedBorderColor' = "#0775a8"

fadeAmount :: Rational
fadeAmount = 0.4

-- Appearance configuration
myConfig = defaultConfig
    { workspaces            = workspaces'
    , modMask               = modMask'
    , borderWidth           = borderWidth'
    , normalBorderColor     = normalBorderColor'
    , focusedBorderColor    = focusedBorderColor'
    , terminal              = terminal'
    , layoutHook            = layoutHook'
    , manageHook            = manageHook'
    }

myDarkXPC :: XPConfig
myDarkXPC = defaultXPConfig
    { font              = "xft:Monaco 9"
    , height            = 28
    , bgColor           = "black"
    , fgColor           = "#684"
    , bgHLight          = "#785"
    , fgHLight          = "black"
    , promptBorderWidth = 0
    , position          = Top
    }

-- Hooks
manageHook' :: ManageHook
manageHook' = foldr1 (<+>)
            [ doF W.swapDown
            , manageHook defaultConfig
            , manageDocks
            , myManageHook
            ]

layoutHook' = avoidStruts
            $ onWorkspaces ["dev", "web"] (noBorders (tabbed shrinkText (theme donaldTheme)) ||| smartBorders threeCols)
            $ onWorkspace "irc" (noBorders Circle ||| smartBorders threeCols ||| noBorders Full ||| noBorders Accordion)
            $ onWorkspace "term" (smartBorders threeCols ||| smartBorders Full ||| smartBorders Accordion)
            $ onWorkspace "im" (noBorders threeCols ||| Grid)
            $ onWorkspace "reading" (smartBorders (tabbed shrinkText (theme donaldTheme)))
            $ onWorkspace "media" (smartBorders Circle ||| smartBorders Full)
            $ onWorkspace "vm" (noBorders Full ||| smartBorders threeCols)
            $ onWorkspace "mail" (noBorders Full ||| smartBorders threeCols)
            $ smartBorders tiled 
    where
    tiled = ResizableTall 1 (2/100) (1/2) []
    threeCols = ThreeCol 1 (3/100) (1/2)

myManageHook = (composeAll . concat)
    [ [className =? name --> doShift wspace | (name, wspace) <- myShifts]
    , [className =? name --> doFloat | name <- myFloats]
    , [className =? "Namoroka" <&&> stringProperty "WM_WINDOW_ROLE" =? "Manager" --> doShift "mail" <+> doF W.focusDown]
    , [className =? "Namoroka" <&&> stringProperty "WM_WINDOW_ROLE" =? "browser" --> doShift "web"  <+> doF W.focusDown]
    , [className =? "Firefox" <&&> stringProperty "WM_WINDOW_ROLE" =? "Manager" --> doShift "mail" <+> doF W.focusDown]
    , [className =? "Firefox" <&&> stringProperty "WM_WINDOW_ROLE" =? "browser" --> doShift "web"  <+> doF W.focusDown]
    , [resource  =? resName --> doIgnore | resName <- myIgnored]
    , [className =? name --> doShift wspace <+> doF W.focusDown | (name, wspace) <- myBackgrounded]
    , [className =? "stalonetray" --> doIgnore]
    , [stringProperty "WM_NAME" =? "ncmpcpp" --> doFloat]
    ]
    where myShifts = [ ("Pidgin", "im")
                     , ("MPlayer", "media")
                     , ("Smplayer", "media")
                     , ("Gvim", "dev")
                     , ("Evince", "reading")
                     , ("Acroread", "reading")
                     , ("IRSSI", "irc")
                     ]
          myFloats = ["Gimp"]
          myIgnored = ["desktop_window", "adl"]
          myBackgrounded = [("uzbl", "web")]
