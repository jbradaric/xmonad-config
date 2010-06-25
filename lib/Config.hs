-------------------------------------------------------------------------- {{{
-- |
-- Module      :  Config
-- Copyright   :  (c) Jurica Bradarić 2010.
-- Maintainer  :  jbradaric at gmail dot com
-- License     :  as-is
-- 
-- Configuration for xmonad
-- 
-- 
-------------------------------------------------------------------------- }}}

module Config where

import XMonad
import XMonad.Prompt
import XMonad.Hooks.DynamicLog

import Hooks

myStatusBar = "dzen2 -e 'entertitle:uncollapse;leavetitle:collapse' -x '0' -h '16' -w '300' -ta l -fg '"
            ++ dzenNormalFG 
            ++ "' -bg '" ++ dzenNormalBG
            ++ "' -fn '" ++ dzenFont ++ "'"

-- | Colors
dzenFont = "Monaco-8"
dzenNormalFG = "#ccccdd"
dzenNormalBG = "#000000"
dzenCurrentFG = "#0b8bff"
dzenCurrentBG = dzenNormalBG
dzenUrgentFG = "#ff0000"
dzenUrgentBG = dzenNormalBG
dzenSeparatorFG = "#333333"
dzenSeparatorBG = dzenNormalBG
dzenLayoutFG = "#222222"
dzenLayoutBG = dzenNormalBG

normalBorderColor'  = "#333333"
focusedBorderColor' = "#0775a8"

dzenSeparator = " | "

-- | Workspaces
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

browser' :: String
browser' = "firefox"

modMask' :: KeyMask
modMask' = mod4Mask

borderWidth' :: Dimension
borderWidth' = 1

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

-- status bar
ppUrgentColor :: String -> String
ppUrgentColor = dzenColor dzenUrgentFG dzenUrgentBG

customPP = dzenPP
    { ppCurrent = dzenColor dzenCurrentFG dzenCurrentBG
    , ppUrgent = ppUrgentColor . trim . dzenStrip . wrap "[" "]"
    , ppSep = dzenColor dzenSeparatorFG dzenSeparatorBG dzenSeparator
    , ppTitle = const ""
    , ppHiddenNoWindows = const ""
    , ppHidden = \s -> if s == "NSP"
                          then ""
                       else wrap " ^bg()^fg(#333333)" " ^fg()" s
    , ppLayout = wrap " " " " . dzenColor dzenLayoutFG dzenLayoutBG
    }
