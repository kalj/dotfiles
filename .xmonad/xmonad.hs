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
import qualified XMonad.StackSet as S
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Hooks.EwmhDesktops (ewmhFullscreen)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Actions.CycleWS
import XMonad.Actions.PhysicalScreens
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar.PP

import Data.Map (fromList, toList)
import Data.Maybe (mapMaybe)
import Data.List (intercalate)

import XMonad.Config.Mate
import XMonad.Layout.NoBorders
import XMonad.Layout.ThreeColumns

import qualified DBus as D
import qualified DBus.Client as D
import XMonad.Hooks.UrgencyHook

-- =============================================================================
-- Main
-- =============================================================================

myConfig dbus = mateConfig {
  modMask    = myModMask
  , manageHook = myManageHook <+> manageHook mateConfig
  , layoutHook = myLayoutHook
  , logHook = logHook mateConfig >> dynamicLogWithPP (appletPP dbus)
  , startupHook = startupHook mateConfig >> setWMName "LG3D"
  , XMonad.keys = myAddKPs  . (XMonad.keys mateConfig)
  } `additionalKeysP` myKeys

main :: IO ()
main = do
  dbus <- D.connectSession
  getWellKnownName dbus
  xmonad $ withUrgencyHook NoUrgencyHook $ ewmhFullscreen $ myConfig dbus

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
         , ("M-x h",            runInTerm "htop")
         , ("M-x f",            spawn "firefox")
         , ("M-x t",            spawn "thunderbird")
         , ("M-x c",            spawn "caja ~")
         , ("M-x e",            spawn "emacs")
         , ("M-<F1>",           spawn "firefox")
         , ("M-<F2>",           spawn "thunderbird")
         , ("C-M-<Insert>",     spawn "playerctl play-pause")
         , ("C-M-<Page_Down>",  spawn "playerctl next")
         , ("C-M-<Page_Up>",    spawn "playerctl previous")
         , ("M-<F11>",          spawn "pactl set-sink-volume 0 -1.5%")
         , ("M-<F12>",          spawn "pactl set-sink-volume 0 +1.5%")
         , ("M-<Print>",        spawn "mate-screenshot -i")
         , ("M-S-s",            spawn "sleep 0.2 ; mate-screenshot -a")
         , ("<XF86Calculator>", runInTerm "octave -q")
         , ("M-c",              spawn "caja ~")
         , ("C-M-S-q",          mateShutdown)
         ]
         ++
         [("M-" ++ mask ++ key, f sc)
         | (key, sc) <- zip ["w", "e", "r"] [0..]
         , (f, mask) <- [(viewScreen horizontalScreenOrderer, ""), (sendToScreen horizontalScreenOrderer, "S-")]]

myKPFilter :: ((ButtonMask, KeySym), X()) -> Maybe ((ButtonMask, KeySym), X())
myKPFilter ((bm,ks),x) | ks==xK_Return     = Just ((bm,xK_KP_Enter),x)
                       | ks==xK_1         = Just ((bm,xK_KP_1),x)
                       | ks==xK_2         = Just ((bm,xK_KP_2),x)
                       | ks==xK_3         = Just ((bm,xK_KP_3),x)
                       | ks==xK_4         = Just ((bm,xK_KP_4),x)
                       | ks==xK_5         = Just ((bm,xK_KP_5),x)
                       | ks==xK_6         = Just ((bm,xK_KP_6),x)
                       | ks==xK_7         = Just ((bm,xK_KP_7),x)
                       | ks==xK_8         = Just ((bm,xK_KP_8),x)
                       | ks==xK_9         = Just ((bm,xK_KP_9),x)
                       | otherwise        = Nothing

myAddKPs = fromList . (\x -> x ++ (mapMaybe myKPFilter x) ) . toList

-- =============================================================================
-- Layout Hook
-- =============================================================================

myLayoutHook = avoidStruts $ smartBorders  $ standardLayouts
  where
    standardLayouts = Tall 1 (3/100) (0.573) ||| ThreeCol 1 (3/100) (1/2) ||| noBorders Full

-- =============================================================================
-- Manage Hook
-- =============================================================================

myManageHook = composeAll
               [ appName   =? "Navigator" --> doShift "1"
               , className =? "chromium-browser" --> doShift "7"
               , className =? "Skype" --> doShift "8"
               , className =? "Transmission-gtk" --> doShift "8"
               , className =? "Element" --> doShift "8"
               , appName   =? "Mail" --> doShift "9"
               , className =? "Wrapper" --> doIgnore
               , className =? "Do" --> doIgnore
               , className =? "Timer-applet" --> doFloat
               , isFullscreen --> doFullFloat
               ]


-- =============================================================================
-- Log hook
-- =============================================================================
getWellKnownName :: D.Client -> IO ()
getWellKnownName dbus = do
  _ <- D.requestName dbus (D.busName_ "org.xmonad.Log")
       [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  return ()

dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal (D.objectPath_ "/org/xmonad/Log") (D.interfaceName_ "org.xmonad.Log") (D.memberName_ "Update")) {
            D.signalBody = [D.toVariant str]
        }
    D.emit dbus signal

appletPP :: D.Client -> PP
appletPP dbus = def
  { ppCurrent       = ("*" ++)
  , ppVisible       = ("+" ++)
  , ppHidden        = ("-" ++)
  , ppUrgent        = ("!" ++)
  , ppRename        = \name ws -> name ++ ":" ++ windowIds ws
  , ppWsSep         = ";"
  , ppSep           = ";"
  , ppLayout        = const ""
  , ppTitle         = ("|" ++) . cropTitle 137
  , ppTitleSanitize = id
  , ppOutput        = dbusOutput dbus
  }
  where windowIds = intercalate "," . map show . S.integrate' . S.stack

cropTitle maxlen wt =  if (length wt) > maxlen then ((take (maxlen-1) wt)++"...") else wt
