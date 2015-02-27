{-# LANGUAGE NoMonomorphismRestriction #-}

import           XMonad
import           XMonad.Actions.GridSelect   hiding (spawnSelected)
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
import           XMonad.Util.Loggers
import           XMonad.Util.Run

import           XMonad.Hooks.FadeWindows

import XMonad.Util.Paste

term, startupWorkspace :: String
term = "terminology"

myWorkspaces :: [String]
myWorkspaces     = [ "1:Term","2:Emacs","3:Web","4:Files","5:Misc" ]

startupWorkspace =   "1:Term"

workspacesC :: [String]
workspacesC = clickable . (map xmEscape) $ myWorkspaces
    where
      clickable l = [ "<action=xdotool key super+" ++ show n ++ ">" ++ ws ++ "</action>" | -- a hack
                      (n,ws) <- zip [1..] l ]
      xmEscape = concatMap doubleLts
          where doubleLts '<' = "<<"
                doubleLts x   = [x]

defaultLayouts =  smartBorders $ avoidStruts $
                    withGaps defaultlayout       |||
                    withGaps ( GridRatio (3/2) ) ||| -- 3 wide, 2 tall

                    Full
  where withGaps = gaps $ zip [U,D,L,R] $ repeat 10

defaultlayout = ResizableTall 1 (3/100) (1/2) []

layouts = onWorkspace "1:Term"  defaultlayout $
          onWorkspace "2:Emacs" Full $
          defaultLayouts
{-
  xprop
    - WM_CLASS(STRING)
  doIgnore
  doFloat
  doF (W.shift "Workspace")
-}

managementHooks :: [ManageHook]
managementHooks =
  [ shiftTo "Emacs"                2
  , shiftTo "xfce4-terminal"       1
  , shiftTo "terminology"          1
  , shiftTo "Thunar"               4
  , shiftTo "MPlayer"              5
  , shiftTo "Google-chrome-stable" 3
  , shiftTo "Firefox"              3
  , className =? "XTerm"           --> doFloat
  ]
  where shiftTo s n = className =? s --> doF ( W.shift $ workspace n)
        workspace n = workspacesC !! (n-1)

myLogHook xmproc = dynamicLogWithPP xmobarPP {
              ppOutput  = hPutStrLn xmproc
            , ppTitle   = xmobarColor "#eeeeee" "" . shorten 50
            , ppCurrent = xmobarColor "#e6744c" "" . wrap "[" "]"
            , ppUrgent  = xmobarColor "#82a6df" "#000000"
            , ppOrder   = \(ws:lay:title:_) -> [ws,title]
            -- ppExtra for stuff
            }

fadeHook :: FadeHook
fadeHook = composeAll [ className =? "Firefox"  --> opaque
                      , transparency 0.2
                      ]

conf xmproc = defaultConfig {
                    focusedBorderColor = "#444444"
                  , normalBorderColor  = "#cccccc"
                  , terminal           = term
                  , borderWidth        = 3
                  , layoutHook         = layouts
                  , workspaces         = workspacesC
                  , modMask            = mod4Mask
                  , handleEventHook    = fadeWindowsEventHook <+> fullscreenEventHook <+> docksEventHook
                  , startupHook        = theStartupHook
                  , manageHook         = manageHook defaultConfig
                                         <+> composeAll managementHooks
                                         <+> manageDocks
                  , logHook            =  fadeWindowsLogHook fadeHook >> myLogHook xmproc
                  }

theStartupHook :: X ()
theStartupHook = do
--  setWMName "LG3D" -- can help java GUI programs work
  windows $ W.greedyView startupWorkspace
--  spawnIfNotRunning "xcompmgr" " -c &"      -- trying to add transparency
  spawnIfNotRunning term ""                 -- start terminal
  spawn $ "feh --bg-scale " ++ background_img_path
    where background_img_path = "~/.xmonad/images/background.*"

spawnIfNotRunning :: String -> String -> X ()   -- uses MonadIO
spawnIfNotRunning cmd args =                    -- another hack
    spawn $ "if [ -z `pgrep " ++ cmd ++ " ` ] ; then " ++ cmd ++ " " ++ args ++ " ;fi"

main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
  xmonad $ withUrgencyHook NoUrgencyHook $ conf xmproc
             `additionalKeysP` [ ("M-S-<Return>", spawn term)
                               , ("M-x o", windows W.focusDown) -- also M-tab
                               , ("M-s", spawnSelected gsconfig
                                         [ ("Terminal", term)
                                         , ("Emacs",    "xdotool key super+2 ; emacs")
                                         , ("Firefox",  "firefox")
                                         , ("Chrome",   "google-chrome-stable")
                                         , ("Files",    "thunar")
                                         ])
                               , ("M-x k", kill)
                               , ("M-l", sendMessage Expand)  -- this is default
                               , ("M-k", sendMessage Shrink)
                               , ("<F6>", spawn "xbacklight -dec 10")
                               , ("<F7>", spawn "xbacklight -inc 10")

                               , ("M-S-r", spawn "killall xcompmgr; sleep 1; xcompmgr -c &")
                               , ("M-v", sendKey noModMask xK_Page_Down)
                               , ("M-S-v", sendKey noModMask xK_Page_Up)
                                 -- restart xcompmgr if stuff freezes
                               ]                           -- M1 is actual alt (xmodmap)
             `removeKeysP` [("M-S-c")]
  where gsconfig = (buildDefaultGSConfig blackColorizer) { gs_cellheight = 40, gs_cellwidth = 75 }

-- ccw from bottom
blackColorizer :: HasColorizer a => a -> Bool -> X (String, String)
blackColorizer str active = return $ if active
                                     then ("white","black") -- background, text
                                     else ("black", "white")

spawnSelected :: GSConfig String -> [(String, String)] -> X () --modified from GS source for full funtionality
spawnSelected conf lst = gridselect conf lst >>= flip whenJust spawn
