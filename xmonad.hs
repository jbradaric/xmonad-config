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
import Hooks

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
