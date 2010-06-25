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

myKeys :: [(String, X ())]
myKeys = concat [ myApps
                , [("C-" ++ show k, focusNth i) | (i, k) <- zip [0..8] [1..]]
                ]
