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

import XMonad.Config.Gnome

-- =============================================================================
-- Misc. variables
-- =============================================================================

myTerminal = "gnome-terminal"
myModMask = mod4Mask
myRun     = gnomeRun

-- =============================================================================
-- Commands and key bindings
-- =============================================================================


myKeys = [ ("M-`", spawn myTerminal)
         , ("M-<Tab>", toggleWS)
         , ("M-<F3>", spawn "firefox")
         , ("M-<F2>", spawn "thunderbird")
         , ("M-<F1>", spawn "pidgin")
         -- , ("M-a", myRun)
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
myLayoutHook = avoidStruts ( layoutHook defaultConfig)

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


-- =============================================================================
-- Main
-- =============================================================================

main = do
  xmonad $ gnomeConfig {
         modMask            = myModMask
       , terminal           = myTerminal
       , manageHook = myManageHook <+> manageHook gnomeConfig
       , layoutHook = myLayoutHook
       , logHook = takeTopFocus
       , startupHook = ewmhDesktopsStartup >> setWMName "LG3D"
       , XMonad.keys = myAddKPEnter  . (XMonad.keys defaultConfig)
    } `additionalKeysP` myKeys

-- . (Data.Map.union (mkKeymap defaultConfig myKeys))
