{-# LANGUAGE FlexibleContexts #-}
-------------------------------------------------------------------------- {{{
-- |
-- Module      :  Utils
-- Copyright   :  (c) Jurica Bradarić 2010.
-- Maintainer  :  jbradaric at gmail dot com
-- License     :  as-is
-- 
-- xmonad configuration. Inspired by mntnoe's configuration found at
-- http://www.mntnoe.com/2010/05/xmonad-config-may-2010/
-- 
-- 
-------------------------------------------------------------------------- }}}

-- misc --
import Graphics.X11.Xlib
import IO (Handle, hPutStrLn) 

-- XMonad stuff
import XMonad

-- utils
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeysP)

-- hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.UrgencyHook

-- extra keys
import Graphics.X11.ExtraTypes.XF86

-- my modules
import Config
import Hooks
import Utils
import Apps
import Keys

-------------------------------------------------------------------------------
-- Main --
main = do
       h <- spawnPipe myStatusBar
       xmonad
        $ withUrgencyHook dzenUrgencyHook { args = [ "-bg"
                                                   , "darkgreen"
                                                   , "-xs"
                                                   , "1"
                                                   ] } 
        $ myConfig
        { logHook = logHook' h } `additionalKeysP` myKeys

logHook' :: Handle ->  X ()
logHook' h = do
    dynamicLogWithPP (customPP { ppOutput = hPutStrLn h })
    fadeInactiveLogHook fadeAmount -- fade inactive windows
    colorFloating focusedBorderColor' -- add borders to floating windows

