--
-- xmonad config file.
--

-- Configuration names
-- {
--       -- simple stuff
--         terminal           = myTerminal,
--         focusFollowsMouse  = myFocusFollowsMouse,
--         borderWidth        = myBorderWidth,
--         modMask            = myModMask,
--         numlockMask        = myNumlockMask,
--         workspaces         = myWorkspaces,
--         normalBorderColor  = myNormalBorderColor,
--         focusedBorderColor = myFocusedBorderColor,

--       -- key bindings
--         keys               = myKeys,
--         mouseBindings      = myMouseBindings,

--       -- hooks, layouts
--         layoutHook         = myLayout,
--         manageHook         = myManageHook,
--         handleEventHook    = myEventHook,
--         logHook            = myLogHook,
--         startupHook        = myStartupHook
--     }

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Util.EZConfig
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.Actions.CycleWS

import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import System.IO
import XMonad.Prompt    
import XMonad.Prompt.Shell
import XMonad.Hooks.UrgencyHook


-- =============================================================================
-- Misc. variables
-- =============================================================================

myTerminal = "urxvtc"
myModMask = mod4Mask

-- =============================================================================
-- Commands and key bindings
-- =============================================================================

sshotCmd="scrot '%F-%T_$wx$h_scrot.png' -e 'mv $f ~/media/pics/sshots/'"
dmenuCmd="exe=`dmenu_path | dmenu -nb '#000' -nf '#AAA' -sb '#AAA' -sf '#000'"++
          " -fn '-misc-fixed-*-*-*-*-12-*-*-*-*-*-*-*'` && eval \"exec $exe\""
volmute = "amixer -q set Master toggle"
volup = "amixer -q set Master 5%+"
voldown = "amixer -q set Master 5%-"
suspnd = "sudo pm-suspend"
hibe = "sudo pm-hibernate"        
pwroff = "sudo poweroff"         
rboot = "sudo reboot"
lckscrn = "xscreensaver-command -lock"

myXPConfig = defaultXPConfig { position = Top
                             -- , font = "xft:Bitstream Vera Sans Mono:pixelsize=10"
                             , font = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
                             , height = 15
                             }
             
myKeys = [ ("M-b", sendMessage ToggleStruts)
         , ("<Print>", spawn sshotCmd)
         , ("C-<Print>", spawn  $ "sleep 0.2; "++ sshotCmd ++ " -s" )
         , ("M-a", spawn dmenuCmd)
         , ("M-x", shellPrompt $ myXPConfig )
         , ("<XF86AudioMute>", spawn volmute)
         , ("<XF86AudioRaiseVolume>", spawn volup)
         , ("<XF86AudioLowerVolume>", spawn voldown)
         , ("<XF86Sleep>", spawn suspnd)
         , ("C-S-<XF86Sleep>", spawn hibe)
         , ("<XF86ScreenSaver>", spawn lckscrn)
         , ("<XF86AudioPlay>", spawn "mpc toggle")
         , ("<XF86AudioStop>", spawn "mpc stop")
         , ("<XF86AudioNext>", spawn "mpc next")
         , ("<XF86AudioPrev>", spawn "mpc prev")
         , ("C-<XF86AudioNext>", spawn "mpc seek +1")
         , ("C-<XF86AudioPrev>", spawn "mpc seek -1")
         , ("C-S-<XF86AudioNext>", spawn "mpc seek +5")
         , ("C-S-<XF86AudioPrev>", spawn "mpc seek -5")
         , ("M-<Tab>", toggleWS)
         , ("M-`", spawn myTerminal)
         , ("M-<F3>", spawn "firefox")
         , ("M-<F4>", spawn "thunderbird")
         ]

-- XF86Sleep
-- XF86ScreenSaver
-- XF86HomePage
-- Help

-- =============================================================================
-- Layout Hook
-- =============================================================================
myLayoutHook = avoidStruts ( layoutHook defaultConfig)

-- =============================================================================
-- Manage Hook
-- =============================================================================

myManageHook = composeAll
               [ className =? "Firefox" --> doShift "1"
               , className =? "Namoroka" --> doShift "1"
               , className =? "Pidgin" --> doShift "8"
               , className =? "Lanikai" --> doShift "9"
               , className =? "Thunderbird" --> doShift "9"
               , manageDocks
               ]


-- =============================================================================
-- Log hook
-- =============================================================================

myLogHook xmproc = dynamicLogWithPP $ xmobarPP
                   { ppOutput = hPutStrLn xmproc
                   , ppTitle = xmobarColor "green" "" . shorten 80
                   , ppUrgent = xmobarColor "yellow" "red" . xmobarStrip
                   }

-- =============================================================================
-- Main
-- =============================================================================

main = do
  xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmonad/xmobar.hs"
  xmonad $ withUrgencyHook NoUrgencyHook
         defaultConfig {
         modMask            = myModMask
       , terminal           = myTerminal
       , normalBorderColor  = "#222222"
       , focusedBorderColor = "#705022"
       , manageHook = myManageHook <+> manageHook defaultConfig
       , layoutHook = myLayoutHook
       , logHook = myLogHook xmproc
       , startupHook = ewmhDesktopsStartup >> setWMName "LG3D"
       } `additionalKeysP` myKeys

