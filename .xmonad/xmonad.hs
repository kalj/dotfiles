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
import Data.Map (fromList, toList)
import Data.Maybe (mapMaybe)

import XMonad.Config.Mate
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.IM
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Reflect

import MaxWidth

-- =============================================================================
-- Misc. variables
-- =============================================================================

myModMask = mod4Mask

-- =============================================================================
-- Commands and key bindings
-- =============================================================================

runInTerm :: String -> X ()
runInTerm command = asks (terminal . config) >>= \t -> spawn $ t  ++ " -x " ++ command


myKeys = [ ("M-`",              spawn (terminal mateConfig ))
         , ("M-<Tab>",          toggleWS)
         , ("M-<F3>",           spawn "firefox")
         , ("M-<F2>",           spawn "thunderbird")
         , ("M-<F1>",           spawn "chromium")
         , ("C-M-<Insert>",     spawn "ncmpcpp toggle")
         , ("C-M-<Page_Down>",  spawn "ncmpcpp next")
         , ("C-M-<Page_Up>",    spawn "ncmpcpp prev")
         , ("<XF86AudioPlay>",    spawn "ncmpcpp toggle")
         , ("<XF86AudioNext>",    spawn "ncmpcpp next")
         , ("<XF86AudioPrev>",    spawn "ncmpcpp prev")
         , ("<XF86AudioStop>",    spawn "ncmpcpp stop")
         , ("<XF86Tools>",        spawn "sonata -p")
         , ("M-x",              runInTerm "top -d 1")
         , ("<XF86Calculator>", runInTerm "octave -q")
         , ("M-c",              spawn "caja ~")
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

myAddKPs = fromList . (\x -> x ++ (mapMaybe myKPFilter x) ) . toList

-- =============================================================================
-- Layout Hook
-- =============================================================================

myLayoutHook = avoidStruts $ smartBorders $ ow "9" mailLayout $ standardLayouts
  where
    standardLayouts = webLayout ||| ThreeCol 1 (3/100) (1/2) ||| noBorders Full
    mailLayout = Tall 1 (3/100) (1/3)
    webLayout = maxWidth [(ClassName "Firefox", 1100)] (Tall 1 (3/100) (0.573))
    ow a b c = onWorkspace a b c

-- =============================================================================
-- Manage Hook
-- =============================================================================

myManageHook = composeAll
               [ appName   =? "Navigator" --> doShift "1"
               , className =? "chromium-browser" --> doShift "7"
               , className =? "Spotify" --> doShift "8"
               , className =? "Skype" --> doShift "8"
               , className =? "Transmission-gtk" --> doShift "8"
               , appName   =? "Mail" --> doShift "9"
               , className =? "Wrapper" --> doIgnore
               , className =? "Do" --> doIgnore
               , className =? "Timer-applet" --> doFloat
               , className =? "Timer-applet" --> doFloat
               , composeOne [ isFullscreen -?> doFullFloat ]
               ]


-- =============================================================================
-- Log hook
-- =============================================================================


-- =============================================================================
-- Main
-- =============================================================================

main = do
  xmonad $ mateConfig {
         modMask            = myModMask
       , manageHook = myManageHook <+> manageHook mateConfig
       , layoutHook = myLayoutHook
       , logHook = logHook mateConfig >> takeTopFocus
       , startupHook = startupHook mateConfig >> setWMName "LG3D"
       , XMonad.keys = myAddKPs  . (XMonad.keys mateConfig)
    } `additionalKeysP` myKeys

-- . (Data.Map.union (mkKeymap gnomeConfig myKeys))
