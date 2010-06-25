module Config where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Prompt

import Hooks

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

myBrowser :: String
myBrowser = "firefox"

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

myScratchpadConf = defaultConfig { terminal = terminal' }
