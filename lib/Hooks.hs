-------------------------------------------------------------------------- {{{
-- |
-- Module      :  Hooks
-- Copyright   :  (c) Jurica Bradarić 2010.
-- Maintainer  :  jbradaric at gmail dot com
-- License     :  as-is
-- 
-- Hooks for xmonad
-- 
-- 
-------------------------------------------------------------------------- }}}
module Hooks where

import XMonad
import qualified XMonad.StackSet as W

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

import XMonad.Util.Scratchpad (scratchpadManageHook)

-- Hooks
manageHook' :: ManageHook
manageHook' = foldr1 (<+>)
            [ doF W.swapDown
            , manageHook defaultConfig
            , manageDocks
            , myManageHook
            , scratchpadManageHook (W.RationalRect 0.4 0.5 0.6 0.3)
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
    , [wmName =? win --> doFloat | win <- scratchpads]
    ]
    where myShifts = [ ("Pidgin", "im")
                     , ("MPlayer", "media")
                     , ("Smplayer", "media")
                     , ("Gvim", "dev")
                     , ("Evince", "reading")
                     , ("Acroread", "reading")
                     ]
          myFloats = ["Gimp"]
          myIgnored = ["desktop_window", "adl"]
          myBackgrounded = [("uzbl", "web")]
          scratchpads = ["ncmpcpp", "irssi", "alsamixer"]
          wmName = stringProperty "WM_NAME"

