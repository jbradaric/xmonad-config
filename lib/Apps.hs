-------------------------------------------------------------------------- {{{
-- |
-- Module      :  Apps
-- Copyright   :  (c) Jurica Bradarić 2010.
-- Maintainer  :  jbradaric at gmail dot com
-- License     :  as-is
-- 
-- Application settings.
-- 
-- 
-------------------------------------------------------------------------- }}}
module Apps (myApps) where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Prompt.AppendFile
import XMonad.Util.Scratchpad (scratchpadSpawnAction)
import XMonad.Hooks.ManageDocks

import Utils
import Config

data App = App
         { key :: String
         , shift :: Bool
         , workspace :: String
         , action :: X ()
         }

nullApp :: App
nullApp = App
        { key = ""
        , shift = False
        , workspace = ""
        , action = return ()
        }

shiftApp :: App
shiftApp = App
         { key = ""
         , shift = True
         , workspace = ""
         , action = return ()
         }

apps = [
         shiftApp -- Firefox
            { key = "M-f"
            , workspace = "web"
            , action = runOrRaise "firefox" (className =? "Namoroka")
            }
       , shiftApp -- Pidgin
            { key = "M-p"
            , workspace = "im"
            , action = spawn "pidgin"
            }
       , shiftApp -- irssi
            { key = "M-i"
            , workspace = "media"
            , action = spawn "urxvtc -title IRSSI -e irssi"
            }
       , nullApp -- previous song on playlist
            { key = "M-<Left>"
            , action = spawn "mpc prev"
            }
       , nullApp -- next song on playlist
            { key = "M-<Right>"
            , action = spawn "mpc next"
            }
        , nullApp -- toggle play/pause
            { key = "M-<Up>"
            , action = spawn "mpc toggle"
            }
        , nullApp -- Volume up
            { key = "<XF86AudioLowerVolume>"
            , action = spawn "amixer -q sset Master 1-"
            }
        , nullApp -- Volume down
            { key = "<XF86AudioRaiseVolume>"
            , action = spawn "amixer -q sset Master 1+"
            }
        , nullApp -- (un)mute the sound
            { key = "<XF86AudioMute>"
            , action = spawn "amixer -q sset Master toggle"
            }
        , nullApp -- lock the computer
            { key = "M-l"
            , action = spawn "slock"
            }
        , nullApp -- add note
            { key = "M-n"
            , action = appendFilePrompt myDarkXPC "/home/m00nblade/NOTES"
            }
        , nullApp -- toggle struts on current workspace
            { key = "M-S-x"
            , action = sendMessage ToggleStruts
            }
        , nullApp -- show scratchpad
            { key = "M-S-t"
            , action = scratchpadSpawnAction myScratchpadConf
            }
        , nullApp -- show dictionary entry for the selection
            { key = "M-d d"
            , action = showDictionary
            }
        , nullApp -- show thesaurus entry for the selection
            { key = "M-d t"
            , action = showThesaurus
            }
        , nullApp -- search the dictionary for the user input
            { key = "M-d m"
            , action = searchDictionary
            }
        , nullApp -- switch conky config
            { key = "M-c s"
            , action = spawn "/home/m00nblade/.scripts/switch_conk.sh"
            }
        , nullApp -- switch the wallpaper
            { key = "M-w s"
            , action = spawn "/home/m00nblade/.scripts/wallpaper.sh"
            }
        , nullApp -- show ncmpcpp
            { key = "M-m"
            , action = runOrRaise' "/usr/bin/urxvtc" ["-geometry", "120x40+100+50", "-name", "ncmpcpp", "-e", "ncmpcpp"] (stringProperty "WM_NAME" =? "ncmpcpp")
            }
       ]

toAction :: App -> (String, X ())
toAction a = case shift a of
                  True -> (key', windows (W.greedyView w') >> action')
                  False -> (key', action')
            where key' = key a
                  action' = action a
                  w' = workspace a

myApps :: [(String, X ())]
myApps = map toAction apps