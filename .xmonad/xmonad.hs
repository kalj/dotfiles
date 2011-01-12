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
import XMonad.Actions.CycleWS
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers


-- =============================================================================
-- Misc. variables
-- =============================================================================

myTerminal = "xterm"
myModMask = mod4Mask
myRun     = ?

-- =============================================================================
-- Commands and key bindings
-- =============================================================================


myKeys = [ ("M-`", spawn myTerminal)
         , ("M-S-<KP_Enter>", spawn myTerminal)
         , ("M-<Tab>", toggleWS)
         , ("M-<F3>", spawn "firefox")
         , ("M-<F2>", spawn "thunderbird")
         , ("M-<F1>", spawn "pidgin")
         , ("M-a", myRun)
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
               , className =? "Rhythmbox" --> doShift "7"
               , appName   =? "spotify.exe" --> doShift "7"
               , className =? "Pidgin" --> doShift "8"
               , className =? "Empathy" --> doShift "8"
               , className =? "Skype" --> doShift "8"
               , className =? "Lanikai" --> doShift "9"
               , className =? "Thunderbird" --> doShift "9"
               , className =? "Do" --> doIgnore
               , composeOne [ isFullscreen -?> doFullFloat ]
               , manageDocks
               ]


-- =============================================================================
-- Log hook
-- =============================================================================


-- =============================================================================
-- Main
-- =============================================================================

main = do
  xmonad $ defaultConfig {
         modMask            = myModMask
       , terminal           = myTerminal
       , manageHook = myManageHook <+> manageHook defaultConfig
       , layoutHook = myLayoutHook
       , startupHook = ewmhDesktopsStartup >> setWMName "LG3D"
       } `additionalKeysP` myKeys

