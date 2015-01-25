{-# LANGUAGE NoMonomorphismRestriction #-}

import           XMonad
import           XMonad.Actions.Plane
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.Gaps
import           XMonad.Layout.Grid
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace  (onWorkspace)
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.SimplestFloat
import           XMonad.Layout.ThreeColumns
import qualified XMonad.StackSet             as W
import           XMonad.Util.EZConfig
import           XMonad.Util.Run

import           Data.Word                   (Word8)
import           Text.Printf
import           XMonad.Actions.GridSelect
import           XMonad.Util.Loggers


term, startupWorkspace :: String
myWorkspaces :: [String]

term = "terminology"

myWorkspaces     = [ "1:Term","2:Emacs","3:Web","4:Files","5:Misc" ]
startupWorkspace =   "1:Term"

workspacesC :: [String]
workspacesC = clickable . (map xmEscape) $ myWorkspaces
    where
      clickable l = [ "<action=xdotool key super+" ++ show n ++ ">" ++ ws ++ "</action>" |
                      (n,ws) <- zip [1..5] l ]
      xmEscape = concatMap doubleLts
          where doubleLts '<' = "<<"
                doubleLts x   = [x]

defaultLayouts =
    smartBorders( avoidStruts(
                              withGaps defaultlayout
                              ||| withGaps ( GridRatio (3/2) ) -- 4 wide, 3 tall
                              ||| Full -- fullscreen
                             ))
        where withGaps = gaps [(U,10),(D,10),(L,10),(R,10)]

defaultlayout = ResizableTall 1 (3/100) (1/2) []

layouts = onWorkspace "1:Term"  defaultlayout $
          onWorkspace "2:Emacs" Full $
          defaultLayouts
-- onWorkspace "9:Pix" gimpLayout $

{-
  xprop
    - WM_CLASS(STRING)
  doIgnore
  doFloat
  doF (W.shift "Workspace")
-}

managementHooks :: [ManageHook]
managementHooks =
    [ className =? "Emacs"                --> doF (W.shift $ workspacesC !! 1)
    , className =? "xfce4-terminal"       --> doF (W.shift $ workspacesC !! 0)
    , className =? "terminology"          --> doF (W.shift $ workspacesC !! 0)
    , className =? "Midori"               --> doF (W.shift $ workspacesC !! 2)
    , className =? "Thunar"               --> doF (W.shift $ workspacesC !! 3)
    , className =? "XTerm"                --> doFloat
    , className =? "openxcom"             --> doF (W.shift $ workspacesC !! 4)
    , className =? "MPlayer"              --> doF (W.shift $ workspacesC !! 4)
    , className =? "Google-chrome-stable" --> doF (W.shift $ workspacesC !! 2)
    , className =? "Netsurf.elf"          --> doF (W.shift $ workspacesC !! 2)
    , className =? "Firefox"              --> doF (W.shift $ workspacesC !! 2)

    ]

myLogHook xmproc = dynamicLogWithPP xmobarPP {
              ppOutput = hPutStrLn xmproc
            , ppTitle = xmobarColor "#eeeeee" "" . shorten 50
            , ppCurrent = xmobarColor "#e6744c" ""
                          . wrap "[" "]"
            , ppUrgent  = xmobarColor "#82a6df" "#000000"
            , ppOrder  = \(ws:lay:title:_) -> [ws,title]
            -- ppExtra for stuff
            }

myConfig xmproc = defaultConfig {
                    focusedBorderColor = "#444444"
                  , normalBorderColor = "#cccccc"
                  , terminal = term
                  , borderWidth = 3
                  , layoutHook = layouts
                  , workspaces = workspacesC
                  , modMask = mod4Mask
                  , handleEventHook = fullscreenEventHook <+> docksEventHook -- added <+>..
                  , startupHook = theStartupHook
                  , manageHook = manageHook defaultConfig
                                 <+> composeAll managementHooks
                                 <+> manageDocks
                  , logHook = myLogHook xmproc
                  }

theStartupHook :: X ()
theStartupHook = do
--  setWMName "LG3D" -- can help java GUI programs work
  windows $ W.greedyView startupWorkspace
  spawnIfNotRunning term ""               -- start terminal
  spawn "xloadimage -onroot -zoom 60 ~/images/background.jpg"

spawnIfNotRunning :: MonadIO m => String -> String -> m ()
spawnIfNotRunning cmd argStr =
    spawn $ "if [ -z `pgrep " ++ cmd ++ " ` ] ; then " ++ cmd ++ " " ++ argStr ++ " ;fi"

main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"

  xmonad $ withUrgencyHook NoUrgencyHook $ myConfig xmproc
             `additionalKeysP` [ ("M-S-<Return>", spawn term)
                               , ("M-x o", windows W.focusDown) -- also M-tab
                               , ("M-s", spawnSelected ( buildDefaultGSConfig blackColorizer )
                                           [ term,"emacs","midori","thunar","xfce4-terminal",
                                                     "google-chrome-stable","firefox"])
                               , ("M-x k", kill)
                               , ("M-l", sendMessage Expand)  -- this is default
                               , ("M-k", sendMessage Shrink)
                               ]                           -- M1 is actual alt (xmodmap)

             `removeKeysP` [("M-S-c")]
-- ccw from bottom
blackColorizer :: HasColorizer a => a -> Bool -> X (String, String)
blackColorizer str active = if active
                            then return ("white","black")
                            else return ("black", "white")
