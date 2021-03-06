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
module Apps 
    ( apps
    , toAction
    ) where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.GridSelect
import XMonad.Prompt.AppendFile
import XMonad.Util.Scratchpad (scratchpadSpawnAction)
import XMonad.Hooks.ManageDocks
import XMonad.Util.NamedScratchpad
import XMonad.Util.XSelection (getSelection)

import XMonad.Prompt (greenXPConfig)
import XMonad.Prompt.Shell

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
shiftApp = nullApp { shift = True }

apps = [
         shiftApp -- Firefox
            { key = "M-f"
            , workspace = "web"
            , action = runOrRaise "/home/m00nblade/.scripts/run_firefox.sh" (className =? "Navigator")
            }
       , shiftApp -- Pidgin
            { key = "M-p"
            , workspace = "im"
            , action = spawn "pidgin"
            }
       , shiftApp -- irssi
            { key = "M-i"
            , action = namedScratchpadAction scratchpads "irssi"
            }
       , nullApp -- previous song on playlist
            { key = "M-<Left>"
            {-, action = spawn "ncmpcpp prev"-}
            , action = spawn "/home/m00nblade/.scripts/mpd_status.sh prev"
            }
       , nullApp -- next song on playlist
            { key = "M-<Right>"
            , action = spawn "/home/m00nblade/.scripts/mpd_status.sh next"
            }
        , nullApp -- toggle play/pause
            { key = "M-<Up>"
            , action = spawn "/home/m00nblade/.scripts/mpd_status.sh play"
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
--        , nullApp -- lock the computer
--            { key = "M-l"
--            , action = spawn "slock"
--            }
        , nullApp -- add note
            { key = "M-n"
            , action = appendFilePrompt myDarkXPC "/home/m00nblade/NOTES"
            }
        , nullApp -- toggle struts on current workspace
            { key = "M-S-x"
            , action = sendMessage ToggleStruts
            }
        , nullApp -- summon terminal
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
            , action = namedScratchpadAction scratchpads "ncmpcpp"
            }
        , nullApp -- show pavucontrol
            { key = "M-a"
            , action = spawn "pavucontrol"
            }
        , nullApp -- show alsamixer
            { key = "M-S-a"
            , action = namedScratchpadAction scratchpads "alsamixer"
            }
        , nullApp -- download selected links with tucan
            { key = "M-z"
            , action = downloadSelection
            }
        , nullApp -- show help
            { key = "M-S-h"
            , action = showHelp
            }
        , nullApp -- show the `run command' prompt
            { key = "M-r"
            , action = shellPrompt myDarkXPC
            }
        , nullApp -- show mutt
            { key = "M-e"
            , action = namedScratchpadAction scratchpads "mutt"
            }
        , nullApp -- show deluge-console
            { key = "M-S-r"
            , action = namedScratchpadAction scratchpads "deluge"
            }
        , nullApp -- set cpu scaling to powersave
            { key = "M-S-p p"
            , action = spawn "/home/m00nblade/.scripts/set_frequency_scaling.sh powersave"
            }
        , nullApp -- set cpu scaling to conservative
            { key = "M-S-p c"
            , action = spawn "/home/m00nblade/.scripts/set_frequency_scaling.sh conservative"
            }
        , nullApp -- show grid selection
            { key = "M-S-e"
            , action = goToSelected defaultGSConfig
            }
       ]

-- | download selected links with tucan
downloadSelection :: X ()
downloadSelection = do
    getSelection >>= (io . writeFile "/tmp/tucan_links.txt")
    namedScratchpadAction scratchpads "tucan"

scratchpads = [ NS "ncmpcpp" ("urxvtc " ++ g ++ " -name ncmpcpp -e ncmpcpp") (wmName =? "ncmpcpp") defaultFloating
              , NS "irssi" ("urxvtc " ++ g ++ " -name irssi -e irssi") (wmName =? "irssi") defaultFloating
              , NS "alsamixer" ("urxvtc " ++ g ++ " -name alsamixer -e alsamixer") (wmName =? "alsamixer") defaultFloating
              , NS "tucan" ("urxvtc " ++ g ++ " -name tucan -e tucan --cli -i /tmp/tucan_links.txt") (wmName =? "tucan") defaultFloating
              , NS "mutt" ("urxvtc " ++ g ++ " -name mutt -e mutt") (wmName =? "mutt") defaultFloating
              , NS "deluge" ("urxvtc " ++ g ++ " -name deluge-console -e deluge-console") (wmName =? "deluge-console") defaultFloating
              ]
              where wmName = stringProperty "WM_NAME"
                    g = "-geometry 102x40+100+50"

-- | Show the keybindings in a dzen2 window
showHelp = spawn $ "dzen2 -l 16 -p -w 700 "
            ++ "-bg 'black' "
            ++ "-fg '#684' "
            ++ "-fn 'Monaco-9' "
            ++ "-x 300 -y 300 "
            ++ "-e 'onstart=scrollhome,uncollapse;"
            ++ "button4=scrollup;"
            ++ "button5=scrolldown;"
            ++ "button1=exit' "
            ++ "< /home/m00nblade/.xmonad/keybindings.txt"

-- | Convert the App entry to a (keybinding, action) pair
toAction :: App -> (String, X ())
toAction a = case shift a of
                  True -> (key', windows (W.greedyView w') >> action')
                  False -> (key', action')
            where key' = key a
                  action' = action a
                  w' = workspace a

