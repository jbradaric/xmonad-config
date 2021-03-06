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
import XMonad.Hooks.ManageHelpers (isDialog, doCenterFloat, doRectFloat)

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
import XMonad.Layout.Reflect
import Data.Ratio((%))

-- utils
import XMonad.Util.Themes (theme,donaldTheme)
import XMonad.Util.NamedScratchpad

scratchpadQuery :: Query Bool
scratchpadQuery = resource =? "scratchpad"

scratchpadManageHook' :: ManageHook
scratchpadManageHook' = namedScratchpadManageHook [NS "" "" scratchpadQuery nonFloating]

-- Hooks
manageHook' :: ManageHook
manageHook' = foldr1 (<+>)
            [ doF W.swapDown
            , manageHook defaultConfig
            , manageDocks
            , myManageHook
            , scratchpadManageHook'
            ]

layoutHook' = avoidStruts
            $ onWorkspaces ["dev", "web"] (noBorders Full ||| noBorders (tabbed shrinkText (theme donaldTheme)) ||| smartBorders threeCols)
            $ onWorkspace "irc" (noBorders gimp ||| noBorders Circle ||| smartBorders threeCols ||| noBorders Full ||| noBorders Accordion)
            $ onWorkspace "term" (smartBorders threeCols ||| smartBorders Full ||| smartBorders Accordion)
            $ onWorkspace "im" (noBorders threeCols ||| Grid ||| withIM (0.7) (Role "conversation") Grid)
            $ onWorkspace "reading" (smartBorders (tabbed shrinkText (theme donaldTheme)))
            $ onWorkspace "media" (smartBorders Circle ||| smartBorders Full)
            $ onWorkspace "vm" (noBorders Full ||| smartBorders threeCols)
            $ onWorkspace "mail" (noBorders Full ||| smartBorders threeCols)
            $ smartBorders tiled 
    where
    tiled = ResizableTall 1 (2/100) (1/2) []
    threeCols = ThreeCol 1 (3/100) (1/2)
    gimp = withIM (0.15) (Role "gimp-toolbox") $
           reflectHoriz $
           withIM (0.15) (Role "gimp-dock") $ reflectHoriz $ layoutHook defaultConfig

myManageHook = (composeAll . concat)
    [ [condition        --> doShift wspace  | (condition, wspace) <- myShifts      ]
    , [wmName =? win    --> doFloat         |           win       <- scratchpads   ]
    , [isDialog         --> doCenterFloat                                          ]
    , [condition        --> doIgnore        |       condition     <- myIgnored     ]
    , [condition        --> doRectFloat (W.RationalRect 0.4 0.4 0.5 0.4)   |       condition     <- myFloats      ]
    ]
    where myShifts = [ (className =? "Pidgin"                           , "im"     )
                     , (className =? "MPlayer"                          , "media"  )
                     , (className =? "Smplayer"                         , "media"  )
                     , (className =? "Gvim"                             , "dev"    )
                     , (className =? "Evince"                           , "reading")
                     , (className =? "gimp"                             , "irc"    )
                     , (className =? "Gimp"                             , "irc"    )
                     , (className =? "deluge"                           , "irc"    )
                     , (className =? "Navigator" <&&> wmRole =? "browser", "web"   )
                     , (className =? "Firefox"  <&&> wmRole =? "browser", "web"    )
                     , (className =? "eclipse" <||> className =? "Eclipse", "dev"  )
                     , (className =? "Nightly" <&&> wmRole =? "Manager", "mail"  )
                     , (className =? "Nightly" <&&> wmRole =? "browser", "web"   )
                     ]
          scratchpads = [ "ncmpcpp"
                        , "irssi"
                        , "alsamixer"
                        , "tucan"
                        , "mutt"
                        ]
          myIgnored = [ resource =? "desktop_window"
                      , resource =? "adl"
                      , className =? "stalonetray"
                      ]
          myFloats = [ className =? "Navigator" <&&> wmRole =? "Manager"
                     , className =? "Firefox"  <&&> wmRole =? "Manager"
                     ] 
          wmName = stringProperty "WM_NAME"
          wmRole = stringProperty "WM_WINDOW_ROLE"
