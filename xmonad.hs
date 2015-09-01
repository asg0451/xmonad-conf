{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
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

import Data.Map (fromList, toList)
import Data.Functor ((<$>))

import XMonad.Hooks.ICCCMFocus

import XMonad.Util.Paste

import XMonad.Layout.Reflect

term, startupWorkspace :: String
term = "urxvt"
-------------------------------------------------------------------------
myWorkspaces :: [String]
myWorkspaces = [ "1:Term", "2:Editor", "3:Web", "4:Files", "5:Misc" ]

startupWorkspace = "1:Term"

workspacesC :: [String]
workspacesC = clickable . map escapeLts $ myWorkspaces
    where
      clickable l = [ "<action=xdotool key super+" ++ show n ++ ">" ++ ws ++ "</action>" | -- a hack
                      (n,ws) <- zip [1 :: Int ..] l ]
      escapeLts   = concatMap ltLt
          where ltLt '<' = "<<"
                ltLt x   = [x]

workspace :: Int -> String
workspace n | n > 0 = workspacesC !! (n-1)
-------------------------------------------------------------------------

defaultLayouts =  smartBorders $ avoidStruts $
                  ResizableTall 1 (3/100) (1/2) []                |||
                  reflectHoriz (ResizableTall 1 (3/100) (1/2) []) |||
                  withGaps defaultlayout                          ||| -- 3 wide, 2 tall
                  Accordion                                       |||
                  Full
withGaps = gaps $ zip [U,D,L,R] $ repeat 10

defaultlayout :: Grid a
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

-- rebind (-->) cause it confuses emacs's syntax highlighting
(->>) :: (Monad m, Monoid a) => m Bool -> m a -> m a
(->>) = (-->)
infix 0 ->>

managementHooks :: [ManageHook]
managementHooks = map (uncurry shiftTo) [("Emacs"                   , 2)
                                        ,("xfce4-terminal"          , 1)
                                        ,("terminology"             , 1)
                                        ,("Thunar"                  , 4)
                                        ,("MPlayer"                 , 5)
                                        ,("Google-chrome-stable"    , 3)
                                        ,("Firefox"                 , 3)
                                        ,("URxvt"                   , 1)
                                        ,("Pavucontrol"             , 5)
                                        ,("libreoffice"             , 4)
                                        ,("libreoffice-startcenter" , 4)
                                        ,("jetbrains-studio"        , 2)
                                        ,("Transmission-gtk"        , 4)
                                        ,("processing-app-Base"     , 2)]
                  <+> [ className =? "XTerm" ->> doFloat
                      , title =? "Hangouts"  ->> doFloat
                      ]

  where shiftTo s n = className =? s ->> doF ( W.shift $ workspace n)

myLogHook xmproc = dynamicLogWithPP xmobarPP {
              ppOutput  = hPutStrLn xmproc
            , ppTitle   = xmobarColor "#eeeeee" "" . shorten 50
            , ppCurrent = xmobarColor "#e6744c" "" . wrap "[" "]"
            , ppUrgent  = xmobarColor "#82a6df" "#000000"
            , ppOrder   = \(ws:lay:title:_) -> [ws,title]
            -- ppExtras for stuff
            }

fadeHook :: FadeHook
fadeHook = composeAll [ className =? "Firefox"  ->> opaque
                      , className =? "URxvt"    ->> transparency 0
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
                                         -- <+> = mappend
                                         -- composeAlll = mconcat
                  , startupHook        = theStartupHook
                  , manageHook         = manageHook defaultConfig
                                         <+> composeAll managementHooks
                                         <+> manageDocks
                  , logHook            =  fadeWindowsLogHook fadeHook >> myLogHook xmproc
                  }

theStartupHook :: X ()
theStartupHook = do
  setWMName "LG3D"
  windows $ W.greedyView startupWorkspace
  spawnIfNotRunning term ""                 -- start terminal
  spawnIfNotRunning "stalonetray" ""        -- now we have a tray
  spawn $ "feh --bg-scale " ++ background_img_path
  spawn "sudo powertop --auto-tune"
    where background_img_path = "~/.xmonad/images/background.*"

spawnIfNotRunning :: (MonadIO m) => String -> String -> m () -- uses MonadIO
spawnIfNotRunning cmd as =                    -- another hack
    spawn $ "if [ -z `pgrep " ++ cmd ++ " ` ] ; then " ++ cmd ++ " " ++ as ++ " ;fi"

main :: IO ()
main = do xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
          xmonad $ withUrgencyHook NoUrgencyHook $ conf xmproc
           -- move between workspaces with arrow keys
            `additionalKeys`  toList (planeKeys mod4Mask (Lines 1) Linear)
            `additionalKeysP` keybindings

keybindings :: [(String, X ())]
keybindings = [ ("M-S-<Return>" , spawn term)   -- M1 is actual alt (xmodmap)
              , ("M-x o"        , windows W.focusDown) -- also M-tab
              , ("M-x l"        , windows W.swapMaster >> windows W.focusDown )
              , ("M-s"          , spawnSelected gsconfig
                          [ ("term"    , term)
                          , ("emacs"   , "xdotool key super+2 ; emacs")
                          , ("firefox" , "firefox")
                          , ("chrome"  , "google-chrome-stable")
                          , ("files"   , "thunar")
                          , ("android" , "android-studio")
                          ])
              , ("M-x k" , kill)
              , ("M-l"   , sendMessage Expand)  -- this is default
              , ("M-k"   , sendMessage Shrink)
              , ("M-i"   , sendMessage MirrorExpand)  -- resize vertically
              , ("M-j"   , sendMessage MirrorShrink)

                -- http://xmonad.org/xmonad-docs/X11/Graphics-X11-Types.html#t:KeySym
              , ("M-w"   , spawn "~/.xmonad/toggle-wifi")

              , ("<F6>"  , spawn "xbacklight -dec 10")
              , ("<F7>"  , spawn "xbacklight -inc 10")
              , ("<F8>"  , spawn "~/.xmonad/toggle-mute")
              , ("<F9>"  , spawn "ponymix decrease 10")
              , ("<F10>" , spawn "ponymix increase 10")

              , ("M-v"   , sendKey noModMask xK_Page_Down)
              , ("M-S-v" , sendKey noModMask xK_Page_Up)
              , ("M-d"   , sendKey noModMask xK_Delete)

              , ("C-M-l" , spawn "~/.xmonad/pixlock")] ++ fnMods

  where gsconfig = (buildDefaultGSConfig blackColorizer) { gs_cellheight = 40, gs_cellwidth = 75 }
        fnMods = [("M-<F" ++ show n ++ ">", sendKey noModMask $ fnKey n) | n <- [1..10]]
        fnKey n = xK_F1 + n - 1 -- uses implementation of xK's as Word64's

-- ccw from bottom
blackColorizer :: HasColorizer a => a -> Bool -> X (String, String)
blackColorizer _ active = return $ if active
                                     then ("white","black") -- background, text
                                     else ("black", "white")

spawnSelected :: GSConfig String -> [(String, String)] -> X () --modified from GS source for full funtionality
spawnSelected gsc lst = gridselect gsc lst >>= flip whenJust spawn
