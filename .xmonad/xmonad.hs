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

import XMonad.Config.Gnome    
import DBus
import DBus.Connection
import DBus.Message
import Control.OldException
import Control.Monad


-- =============================================================================
-- Misc. variables
-- =============================================================================

myTerminal = "gnome-terminal"
myModMask = mod4Mask

-- =============================================================================
-- Commands and key bindings
-- =============================================================================

myKeys = [ ("M-a", gnomeRun)
         , ("M-S-<KP_Enter>", spawn myTerminal)
         , ("M-<Tab>", toggleWS)
         , ("M-`", spawn myTerminal)
         ]

-- =============================================================================
-- Layout Hook
-- =============================================================================

-- =============================================================================
-- Manage Hook
-- =============================================================================

myManageHook = composeAll
               [ className =? "Firefox" --> doShift "1"
               , className =? "Namoroka" --> doShift "1"
               , className =? "Pidgin" --> doShift "8"
               , className =? "Lanikai" --> doShift "9"
               , className =? "Thunderbird" --> doShift "9"
               ]


-- =============================================================================
-- Log hook
-- =============================================================================

pangoColor :: String -> String -> String
pangoColor fg = wrap left right
    where
      left = "<span foreground=\"" ++ fg ++ "\">"
      right = "</span>"

sanitize :: String -> String
sanitize [] = []
sanitize (x:rest) | fromEnum x > 127 = "&#" ++ show (fromEnum x) ++ "; " ++ sanitize rest
                  | otherwise 	   = x : sanitize rest

myLogHook dbus = dynamicLogWithPP $ defaultPP {
                   ppOutput		= \ str -> do
                                            let str' = "<span font=\"Terminus 9 Bold\">" ++ str ++ "</span>"
                                            msg <- newSignal "/org/xmonad/Log" "org.xmonad.Log" "Update"
                                            addArgs msg [String str']
                                            --If the send fails, ignore it.
                                            send dbus msg 0 `catchDyn` (\ (DBus.Error _name _msg) -> return 0)
                                            return ()
                 , ppTitle = pangoColor "#003366" . shorten 100
                 , ppCurrent = pangoColor "#006666" . wrap "[" "]"
                 , ppVisible = pangoColor "#663366" . wrap "(" ")"
                 , ppHidden = wrap " " " "
                 , ppUrgent = pangoColor "red"
                 }

-- =============================================================================
-- Main
-- =============================================================================

-- This retry is really awkward, but sometimes DBus won't let us get our
-- name unless we retry a couple times.
getWellKnownName :: Connection -> IO ()
getWellKnownName dbus = tryGetName `catchDyn` (\ (DBus.Error _ _) -> getWellKnownName dbus)
    where tryGetName = do
            namereq <- newMethodCall serviceDBus pathDBus interfaceDBus "RequestName"
            addArgs namereq [String "org.xmonad.Log", Word32 5]
            sendWithReplyAndBlock dbus namereq 0
            return ()

main = withConnection Session $ \ dbus -> do
         putStrLn "Getting well-known name."
         getWellKnownName dbus
         putStrLn "Got name, starting XMonad."
         xmonad $ gnomeConfig {
                      modMask = myModMask
                    , terminal = myTerminal
                    , manageHook = myManageHook <+> manageHook gnomeConfig
                    , logHook = myLogHook dbus
                    , startupHook = ewmhDesktopsStartup >> setWMName "LG3D"                                
                    } `additionalKeysP` myKeys
