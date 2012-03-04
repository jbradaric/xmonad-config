-------------------------------------------------------------------------- {{{
-- |
-- Module      :  Keys
-- Copyright   :  (c) Jurica Bradarić 2010.
-- Maintainer  :  jbradaric at gmail dot com
-- License     :  as-is
-- 
-- Keybindings
-- 
-- 
-------------------------------------------------------------------------- }}}
module Keys (myKeys) where

import XMonad

import Apps
import Config
import XMonad.Actions.FocusNth (focusNth)
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect (goToSelected, defaultGSConfig)

myKeys :: [(String, X ())]
{-
 - myKeys = concat [ myApps
 -                 , [("C-" ++ show k, focusNth i) | (i, k) <- zip [0..8] [1..]]
 -                 ]
 -                 where myApps = map toAction apps
 -}
--myKeys = concat [ map toAction apps
--                , [("M-"++key, screenWorkspace sc >>= flip whenJust (windows . f)) 
--                    | (key, sc) <- zip ["w", "v"] [0..]
--                    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
--                  ]
--                ]
myKeys = concat [ map toAction apps
                , [ ("M-j", nextScreen), ("M-S-j", swapNextScreen), ("M-g", goToSelected defaultGSConfig)]
                ]
