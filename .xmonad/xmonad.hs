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
import XMonad.Hooks.EwmhDesktops (ewmhDesktopsStartup)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.ICCCMFocus (takeTopFocus)
import XMonad.Actions.CycleWS
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import Data.Map (fromList, toList)
import Data.Maybe (isJust, catMaybes, mapMaybe, fromMaybe, listToMaybe)
import Data.List (intersperse)

import XMonad.Config.Mate
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.IM
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Reflect

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8
import Codec.Binary.UTF8.String (encodeString)
import XMonad.Hooks.UrgencyHook
import XMonad.Util.NamedWindows
import XMonad.Hooks.DynamicLog
import XMonad.Util.WorkspaceCompare (WorkspaceSort)

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
         , ("M-<F1>",           spawn "firefox")
         , ("M-<F2>",           spawn "thunderbird")
         , ("M-<F3>",           spawn "chromium")
         , ("C-M-<Insert>",     spawn "ncmpcpp toggle")
         , ("C-M-<Page_Down>",  spawn "ncmpcpp next")
         , ("C-M-<Page_Up>",    spawn "ncmpcpp prev")
         , ("M-<F11>",            spawn "pactl set-sink-volume 0 -1.5%")
         , ("M-<F12>",            spawn "pactl set-sink-volume 0 +1.5%")
         , ("M-<Print>",            spawn "mate-screenshot -i")
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
getWellKnownName :: D.Client -> IO ()
getWellKnownName dbus = do
  D.requestName dbus (D.busName_ "org.xmonad.Log")
                [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  return ()

dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal (D.objectPath_ "/org/xmonad/Log") (D.interfaceName_ "org.xmonad.Log") (D.memberName_ "Update")) {
--             D.signalBody = [D.toVariant ("<b>" ++ (UTF8.decodeString str) ++ "</b>")]
            D.signalBody = [D.toVariant ((UTF8.decodeString str))]
        }
    D.emit dbus signal

--
-- iconsAppletLog based on dynamicLogWithPP
-- (in addition to workspace numbers, it also outputs window id-s)
--

iconsAppletLog :: D.Client -> X ()
iconsAppletLog dbus = iconsAppletLogString >>= io . (dbusOutput dbus)

iconsAppletLogString :: X String
iconsAppletLogString = do

    winset <- gets windowset
    urgents <- readUrgents
    sort' <- ppSort iconsAppletPP

    -- layout description
    let ld = description . S.layout . S.workspace . S.current $ winset

    -- workspace list
    let ws = iconsAppletWindowSet sort' urgents winset

    -- window title
    wt <- maybe (return "") (fmap show . getName) . S.peek $ winset

    -- run extra loggers, ignoring any that generate errors.
    extras <- mapM (flip catchX (return Nothing)) $ ppExtras iconsAppletPP

    return $ encodeString . sepBy (ppSep iconsAppletPP) . ppOrder iconsAppletPP $
                        [ ws
                        , ppLayout iconsAppletPP ld
                        , ppTitle iconsAppletPP wt
                        ]
                        ++ catMaybes extras

iconsAppletWindowSet :: WorkspaceSort -> [Window] -> WindowSet -> String
iconsAppletWindowSet sort' urgents s = sepBy ";" . map fmt . sort' $
            map S.workspace (S.current s : S.visible s) ++ S.hidden s
   where this     = S.currentTag s
         visibles = map (S.tag . S.workspace) (S.visible s)

         fmt w = printer iconsAppletPP (iconsAppletFormatWorkspace w)
          where printer | any (\x -> maybe False (== S.tag w) (S.findTag x s)) urgents  = ppUrgent
                        | S.tag w == this                                               = ppCurrent
                        | S.tag w `elem` visibles                                       = ppVisible
                        | isJust (S.stack w)                                            = ppHidden
                        | otherwise                                                     = ppHiddenNoWindows

iconsAppletFormatWorkspace :: S.Workspace WorkspaceId (Layout Window) Window -> String
iconsAppletFormatWorkspace w = (S.tag w) ++ ":" ++ (iconsAppletFormatStack . S.stack $ w)

iconsAppletFormatStack :: Maybe (S.Stack Window) -> String
iconsAppletFormatStack s = case s of
  Just value -> sepBy "," . map show $ (reverse (S.up value) ++ [S.focus value] ++ S.down value)
  Nothing -> ""

iconsAppletPP :: PP
iconsAppletPP = defaultPP {
    ppTitle    = ("|" ++)
  , ppCurrent  = ("*" ++)
  , ppVisible  = ("+" ++)
  , ppHidden   = ("-" ++)
  , ppUrgent   = ("!" ++)
  , ppLayout   = const ""
  , ppSep      = ";"
  }

sepBy :: String   -- ^ separator
      -> [String] -- ^ fields to output
      -> String
sepBy sep = concat . intersperse sep . filter (not . null)


-- =============================================================================
-- Main
-- =============================================================================

myConfig dbus = mateConfig {
  modMask    = myModMask
  , manageHook = myManageHook <+> manageHook mateConfig
  , layoutHook = myLayoutHook
  , logHook = logHook mateConfig >> takeTopFocus >> iconsAppletLog dbus
  , startupHook = startupHook mateConfig >> setWMName "LG3D"
  , XMonad.keys = myAddKPs  . (XMonad.keys mateConfig)
  } `additionalKeysP` myKeys

-- . (Data.Map.union (mkKeymap gnomeConfig myKeys))

main :: IO ()
main = do
  dbus <- D.connectSession
  getWellKnownName dbus
  xmonad $ myConfig dbus
