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
import Hooks
import Utils

-------------------------------------------------------------------------------
-- Main --
main = do
       --h <- spawnPipe "xmobar" 
       h <- spawnPipe myStatusBar
       --conky <- spawnPipe myConky
       xmonad $ withUrgencyHook dzenUrgencyHook { args = ["-bg", "darkgreen", "-xs", "1"] } $ myConfig
        { logHook = logHook' h } `additionalKeysP` myKeys


logHook' :: Handle ->  X ()
logHook' h = do
    dynamicLogWithPP (customPP { ppOutput = hPutStrLn h })
    fadeInactiveLogHook fadeAmount -- fade inactive windows
    colorFloating focusedBorderColor' -- add borders to floating windows


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
         , ("M-d d", showDictionary)
         , ("M-d t", showThesaurus)
         , ("M-d m", searchDictionary)
         , ("M-c s", spawn "/home/m00nblade/.scripts/switch_conk.sh") -- switch conky configuration to match the running network interface
         , ("M-w s", spawn "/home/m00nblade/.scripts/wallpaper.sh") -- switch to a random wallpaper
         , ("M-m", runOrRaise' "/usr/bin/urxvtc" ["-geometry", "120x40+100+50", "-name", "ncmpcpp", "-e", "ncmpcpp"] (stringProperty "WM_NAME" =? "ncmpcpp")) -- XXX
         ]
         {- focus the nth window with <Ctrl>-# -}
         , [ ("C-" ++ show k, focusNth i) | (i, k) <- zip [0..8] [1..]]
        ]
