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
import XMonad.Util.EZConfig
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ICCCMFocus
import XMonad.Actions.CycleWS
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import Data.Map
import Maybe

import XMonad.Hooks.DynamicLog
import XMonad.Util.Run(spawnPipe)
import System.IO
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Hooks.UrgencyHook

-- =============================================================================
-- Misc. variables
-- =============================================================================

myTerminal = "uxterm"
myModMask = mod4Mask
myRun     = spawn dmenuCmd

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

toexternal = "/home/kalle/bin/to-x"
tointernal = "/home/kalle/bin/from-x"

myXPConfig = defaultXPConfig { position = Top
                             -- , font = "xft:Bitstream Vera Sans Mono:pixelsize=10"
                             , font = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
                             , height = 15
                             }

myKeys = [ ("M-`", spawn myTerminal)
         , ("M-<Tab>", toggleWS)
         , ("M-<F3>", spawn "firefox")
         , ("M-<F2>", spawn "thunderbird")
         , ("M-<F1>", spawn "pidgin")
         , ("M-a", myRun)

         -- arch linux specifics
         , ("M-b", sendMessage ToggleStruts)
         , ("M-x", shellPrompt $ myXPConfig )
         , ("<Print>", spawn sshotCmd)
         , ("C-<Print>", spawn  $ "sleep 0.2; "++ sshotCmd ++ " -s" )
         , ("C-M-<Up>", spawn volup)
         , ("C-M-<Down>", spawn voldown)
         , ("C-M-l", spawn lckscrn)
         , ("C-M-<Insert>", spawn "mpc toggle")
         , ("C-M-<End>", spawn "mpc stop")
         , ("C-M-<Page_Down>", spawn "mpc next")
         , ("C-M-<Page_Up>", spawn "mpc prev")
         , ("C-M-<Right>", spawn "mpc seek +1")
         , ("C-M-<Left>", spawn "mpc seek -1")
         , ("C-M-S-<Right>", spawn "mpc seek +5")
         , ("C-M-S-<Left>", spawn "mpc seek -5")
         -- laptop keyboard specifics
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
         , ("C-M-<F4>", spawn toexternal)
         , ("M-<F4>", spawn tointernal)
         ]

myKPEnterFilter :: ((ButtonMask, KeySym), X()) -> Maybe ((ButtonMask, KeySym), X())
myKPEnterFilter ((bm,apa),x) = case apa of
  xK_Return -> Just ((bm,xK_KP_Enter),x)
  otherwise -> Nothing

myAddKPEnter = Data.Map.fromList . (\x -> x ++ (Maybe.mapMaybe myKPEnterFilter x) ) . Data.Map.toList

-- XF86Sleep
-- XF86ScreenSaver
-- XF86HomePage
-- Help

-- =============================================================================
-- Layout Hook
-- =============================================================================
myLayoutHook = avoidStruts ( layoutHook gnomeConfig)

-- =============================================================================
-- Manage Hook
-- =============================================================================

myManageHook = composeAll
               [ appName   =? "Navigator" --> doShift "1"
               , className =? "Rhythmbox" --> doShift "7"
               , appName   =? "spotify.exe" --> doShift "7"
               , className =? "Spotify" --> doShift "7"
               , className =? "Pidgin" --> doShift "8"
               , className =? "Empathy" --> doShift "8"
               , className =? "Skype" --> doShift "8"
               , appName   =? "Mail" --> doShift "9"
               , className =? "Do" --> doIgnore
               , className =? "Timer-applet" --> doFloat
               , composeOne [ isFullscreen -?> doFullFloat ]
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
       , manageHook = myManageHook <+> manageHook defaultConfig
       , layoutHook = myLayoutHook
       , logHook = do myLogHook xmproc
                      takeTopFocus
       , startupHook = ewmhDesktopsStartup >> setWMName "LG3D"
       , XMonad.keys = myAddKPEnter  . (XMonad.keys gnomeConfig)
    } `additionalKeysP` myKeys

-- . (Data.Map.union (mkKeymap gnomeConfig myKeys))
