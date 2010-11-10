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

myTerminal = "xterm"
myModMask = mod4Mask

-- =============================================================================
-- Commands and key bindings
-- =============================================================================


myKeys = [ ("M-`", spawn myTerminal)
         , ("M-S-<KP_Enter>", spawn myTerminal)
         , ("M-<Tab>", toggleWS)
         , ("M-<F3>", spawn "firefox")
         , ("M-<F4>", spawn "thunderbird")
         , ("M-<F5>", spawn "pidgin")
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
               , className =? "spotify.exe" --> doShift "8"
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

