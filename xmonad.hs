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
import           XMonad.Layout.Accordion (Accordion(..))
import           XMonad.Hooks.FadeWindows

import XMonad.Util.Paste

term, startupWorkspace :: String
term = "urxvt"
-------------------------------------------------------------------------
myWorkspaces :: [String]
myWorkspaces = [ "1:Term","2:Emacs","3:Web","4:Files","5:Misc" ]

startupWorkspace = "1:Term"

workspacesC :: [String]
workspacesC = clickable . map escapeLts $ myWorkspaces
    where
      clickable l = [ "<action=xdotool key super+" ++ show n ++ ">" ++ ws ++ "</action>" | -- a hack
                      (n,ws) <- zip [1..] l ]
      escapeLts   = concatMap ltLt
          where ltLt '<' = "<<"
                ltLt x   = [x]

workspace :: Int -> String
workspace n | n > 0 = workspacesC !! (n-1)
-------------------------------------------------------------------------


defaultLayouts =  smartBorders $ avoidStruts (
                                              ResizableTall 1 (3/100) (1/2) [] |||
                                              withGaps defaultlayout           ||| -- 3 wide, 2 tall
                                              Accordion                        |||
                                              Full)
withGaps = gaps $ zip [U,D,L,R] $ repeat 10

defaultlayout =  GridRatio (3/2)

layouts =
  onWorkspace (workspace 1) (bs defaultlayout) $
          defaultLayouts
    --          onWorkspace (workspace 2) (bs Full) $
    where bs = smartBorders . avoidStruts

{-
  xprop
    - WM_CLASS(STRING)
  doIgnore
  doFloat
  doF (W.shift "Workspace")
-}

managementHooks :: [ManageHook]
managementHooks =
  [ "Emacs"                `shiftTo` 2
  , "xfce4-terminal"       `shiftTo` 1
  , "terminology"          `shiftTo` 1
  , "Thunar"               `shiftTo` 4
  , "MPlayer"              `shiftTo` 5
  , "Google-chrome-stable" `shiftTo` 3
  , "Firefox"              `shiftTo` 3
  , "URxvt"                `shiftTo` 1
  , "Pavucontrol"          `shiftTo` 5
  , className =? "XTerm"   --> doFloat
  , title =? "Hangouts"    --> doFloat  -- hangouts floaty thing
  ]
  where shiftTo s n = className =? s --> doF ( W.shift $ workspace n)


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
                      , className =? "URxvt"    --> transparency 0
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
                               , ("M-x l", do   -- nice!
                                    windows W.swapMaster
                                    windows W.focusDown
                                 )
                               , ("M-s", spawnSelected gsconfig
                                         [ ("term"        , term)
                                         , ("emacs"       , "xdotool key super+2 ; emacs")
                                         , ("firefox"     , "firefox")
                                         , ("chrome"      , "google-chrome-stable")
                                         , ("word"       , "libreoffice")
                                         ])
                               , ("M-x k" , kill)
                               , ("M-l"   , sendMessage Expand)  -- this is default
                               , ("M-k"   , sendMessage Shrink)
                               , ("<F6>"  , spawn "xbacklight -dec 10")
                               , ("<F7>"  , spawn "xbacklight -inc 10")

                               , ("M-S-r" , spawn "killall xcompmgr; sleep 1; xcompmgr -c &")
                               , ("M-v"   , sendKey noModMask xK_Page_Down)
                               , ("M-S-v" , sendKey noModMask xK_Page_Up)
                               , ("M-d",    sendKey noModMask xK_Delete)
                               ]                           -- M1 is actual alt (xmodmap)
             `removeKeysP` ["M-S-c"]
  where gsconfig = (buildDefaultGSConfig blackColorizer) { gs_cellheight = 40, gs_cellwidth = 75 }

-- ccw from bottom
blackColorizer :: HasColorizer a => a -> Bool -> X (String, String)
blackColorizer str active = return $ if active
                                     then ("white","black") -- background, text
                                     else ("black", "white")

spawnSelected :: GSConfig String -> [(String, String)] -> X () --modified from GS source for full funtionality
spawnSelected conf lst = gridselect conf lst >>= flip whenJust spawn
