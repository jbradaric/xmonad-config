{-# LANGUAGE FlexibleContexts #-}
-------------------------------------------------------------------------- {{{
-- |
-- Module      :  Utils
-- Copyright   :  (c) Jurica Bradarić 2010.
-- Maintainer  :  jbradaric at gmail dot com
-- License     :  as-is
-- 
-- Utility functions
-- 
-- 
-------------------------------------------------------------------------- }}}
module Utils
    ( runOrRaise'
    , colorFloating
    , showDictionary
    , showThesaurus
    , searchDictionary
    ) where


import Config (myDarkXPC)
import XMonad
import qualified XMonad.StackSet as W

import Control.Monad (when)
import qualified Data.Map as M

import XMonad.Actions.WindowGo (raiseMaybe)

import XMonad.Util.Run (spawnPipe, runInTerm, safeSpawn)
import XMonad.Util.XSelection (getSelection)
import XMonad.Prompt.Input (inputPrompt)

-- | Run or raise an application with arguments
runOrRaise' :: FilePath -> [String] -> Query Bool -> X ()
runOrRaise' cmd args query = raiseMaybe (safeSpawn cmd args) query

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

-- | Add borders to floating windows
colorFloating :: String -> X ()
colorFloating = colorWhen isFloat

{- dictionary and thesaurus to use for word lookup -}
myDictionary, myThesaurus :: String
myDictionary = " -d wn "
myThesaurus = " -d moby-thesaurus "

-- | Show a dictionary explanation for the user selection
showDictionary = getSelection >>= spawnDictionary myDictionary
showThesaurus = getSelection >>= spawnDictionary myThesaurus

{-
 - The arguments to the sed utility. Used to highlight some words in
 - the output of dict.
 -}
sedArgs :: String
sedArgs =  "-e '1,4d' "
        ++ "-e 's/{\\([^}]*\\)}/^fg(white)\\1^fg()/g' "
        ++ "-e 's/\\(\\[syn:\\)/^fg(#ccd)\\1^fg()/g' "

dictBgColor = "black"
dictFgColor = "#684"

{-
 - Show a dzen2 window with the output of the dict program for
 - the given word
 -}
spawnDictionary :: String -> String -> X ()
spawnDictionary args word = spawn $ "dict " 
                               ++ args ++ " '" ++ word ++ "' "
                               ++ " | sed " ++ sedArgs
                               ++ " | dzen2 -l 16 -p -w 700 "
                               ++ "-bg '" ++ dictBgColor ++ "' "
                               ++ "-fg '" ++ dictFgColor ++ "' " 
                               ++ "-fn 'Monaco-9' "
                               ++ "-x 300 -y 300 "
                               ++ "-e 'onstart=scrollhome,uncollapse;"
                               ++ "button4=scrollup;"
                               ++ "button5=scrolldown;"
                               ++ "button1=exit'"

-- | Search the dictionary for the word the user entered.
searchDictionary = inputPrompt myDarkXPC "Dictionary search" >>= spawnDictionary' myDictionary
    where spawnDictionary' dict word = case word of
              Just word -> spawnDictionary dict word
              Nothing -> return ()
