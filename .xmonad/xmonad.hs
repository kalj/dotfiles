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
import Data.Maybe

import XMonad.Config.Gnome
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.IM
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Reflect

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
         , ("C-M-<Insert>", spawn "ncmpcpp toggle")
         , ("C-M-<Page_Down>", spawn "ncmpcpp next")
         , ("C-M-<Page_Up>", spawn "ncmpcpp prev")
         -- , ("M-a", myRun)
         ]

myKPFilter :: ((ButtonMask, KeySym), X()) -> Maybe ((ButtonMask, KeySym), X())
myKPFilter ((bm,apa),x) = case apa of
  xK_Return     -> Just ((bm,xK_KP_Enter),x)
  xK_1          -> Just ((bm,xK_KP_1),x)
  xK_2          -> Just ((bm,xK_KP_2),x)
  xK_3          -> Just ((bm,xK_KP_3),x)
  xK_4          -> Just ((bm,xK_KP_4),x)
  xK_5          -> Just ((bm,xK_KP_5),x)
  xK_6          -> Just ((bm,xK_KP_6),x)
  xK_7          -> Just ((bm,xK_KP_7),x)
  xK_8          -> Just ((bm,xK_KP_8),x)
  xK_9          -> Just ((bm,xK_KP_9),x)
  otherwise     -> Nothing

myAddKPs = Data.Map.fromList . (\x -> x ++ (Data.Maybe.mapMaybe myKPFilter x) ) . Data.Map.toList

-- XF86Sleep
-- XF86ScreenSaver
-- XF86HomePage
-- Help

-- =============================================================================
-- Layout Hook
-- =============================================================================

myLayoutHook = avoidStruts $ smartBorders $ ow "8" imLayout $ ow "9" mailLayout $ ow "1" webLayout $ standardLayouts
  where
    standardLayouts = Tall 1 (3/100) (0.56) ||| ThreeCol 1 (3/100) (1/2) ||| Full
    imLayout = reflectHoriz $ withIM (1/7) (Role "buddy_list") $ reflectHoriz $ webLayout
    mailLayout = Tall 1 (3/100) (1/3)
    webLayout = Tall 1 (3/100) (0.60)
    ow a b c = onWorkspace a b c


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
               , className =? "Transmission-gtk" --> doShift "8"
               , appName   =? "Mail" --> doShift "9"
               , className =? "Do" --> doIgnore
               , className =? "Gimp-2.6" --> doFloat
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
       , logHook = logHook gnomeConfig >> takeTopFocus
       , startupHook = ewmhDesktopsStartup >> setWMName "LG3D"
       , XMonad.keys = myAddKPs  . (XMonad.keys gnomeConfig)
    } `additionalKeysP` myKeys

-- . (Data.Map.union (mkKeymap gnomeConfig myKeys))
