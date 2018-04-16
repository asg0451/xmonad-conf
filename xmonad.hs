{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import           Data.Map                    (fromList, toList)
import           Data.Monoid
import           XMonad
import           XMonad.Actions.GridSelect   hiding (spawnSelected)
import           XMonad.Actions.Plane
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.FadeWindows
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.Accordion     (Accordion (..))
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.Gaps
import           XMonad.Layout.Grid
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace  (onWorkspace)
import           XMonad.Layout.Reflect
import           XMonad.Layout.ResizableTile
import           XMonad.Prompt
import           XMonad.Prompt.Shell
import qualified XMonad.StackSet             as W
import           XMonad.Util.EZConfig
import           XMonad.Util.NamedWindows
import           XMonad.Util.Paste
import           XMonad.Util.Run

import           Control.Monad
import           Data.Maybe
import           System.Environment
import           System.Exit
import           System.Process              (rawSystem)
import           XMonad.Actions.CycleWS

-- lib
import           Command                     (defaultCommands, runCommand)

-- https://pbrisbin.com/posts/using_notify_osd_for_xmonad_notifications/
data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
  urgencyHook LibNotifyUrgencyHook w =
    do name <- getName w
       Just idx <- W.findTag w <$> gets windowset
       safeSpawn "notify-send" [show name, "workspace " ++ idx]

term, startupWorkspace :: String
term = "terminology"
-------------------------------------------------------------------------
myWorkspaces :: [String]
myWorkspaces = [ "1:Term", "2:Editor", "3:Web", "4:Music", "5:Misc" ]

startupWorkspace = "1:Term"

workspacesC :: [String]
workspacesC = clickable . map escapeLts $ myWorkspaces
  where
    clickable l =
        ["<action=xdotool key super+" ++ show n ++ ">" ++ ws ++ "</action>"   -- a hack
          | (n,ws) <- zip [1 :: Int ..] l]
    escapeLts = concatMap ltLt
      where
        ltLt '<' = "<<"
        ltLt x = [x]

workspace :: Int -> String
workspace n | n > 0 = workspacesC !! (n-1)
-------------------------------------------------------------------------

defaultLayouts =  smartBorders $ avoidStruts $
                  ResizableTall 1 (3/100) (1/2) []                |||
                  withGaps defaultlayout                          ||| -- 3 wide, 2 tall
                  Full
withGaps = gaps $ zip [U,D,L,R] $ repeat 10

defaultlayout :: Grid a
defaultlayout =  GridRatio (3/2)

layouts = onWorkspace (workspace 1) (bs defaultlayout) defaultLayouts
  where
    --          onWorkspace (workspace 2) (bs Full) $
    bs = smartBorders . avoidStruts

{-
  xprop
    - WM_CLASS(STRING)
  doIgnore
  doFloat
  doF (W.shift "Workspace")
-}

managementHooks :: [ManageHook]
managementHooks =
    map
        (uncurry shiftTo)
        [ ("Emacs", 2)
        , ("xfce4-terminal", 1)
        , ("terminology", 1)
        , ("Thunar", 4)
        , ("MPlayer", 5)
        , ("google-chrome", 3) -- chrome's classname changed
        , ("Firefox", 3)
        , ("URxvt", 1)
        , ("Pavucontrol", 5)
        , ("libreoffice", 4)
        , ("libreoffice-startcenter", 4)
        , ("jetbrains-studio", 2)
        , ("Transmission-gtk", 4)
        , ("openmw", 5) -- morrowind
        , ("Spotify", 4)
        , ("processing-app-Base", 2)] <>
    [(== "XTerm") <$> className --> doFloat, (== "Hangouts") <$> title --> doFloat]
  where
    shiftTo s n = (== s) <$> className --> doF (W.shift $ workspace n)


myLogHook xmproc =
    dynamicLogWithPP
        xmobarPP
        { ppOutput = hPutStrLn xmproc
        , ppTitle = xmobarColor "#eeeeee" "" . shorten 50
        , ppCurrent = xmobarColor "#e6744c" "" . wrap "[" "]"
        , ppUrgent = xmobarColor "#82a6df" "#000000"
        , ppOrder = \(ws:lay:title:_) ->
                         [ws, title]-- ppExtras for stuff
        }

fadeHook :: FadeHook
fadeHook =
    mconcat
        [ className =? "Firefox" --> opaque -- q =? s is (== s) <$> q
        , className =? "URxvt" --> transparency 0
        , transparency 0.2]

conf xmproc =
    def
    { focusedBorderColor = "#444444"
    , normalBorderColor = "#cccccc"
    , terminal = term
    , borderWidth = 1
    , layoutHook = layouts
    , workspaces = workspacesC
    , modMask = mod4Mask
    , handleEventHook = fadeWindowsEventHook <> fullscreenEventHook <> docksEventHook
    , startupHook = theStartupHook
    , manageHook = manageHook def <> mconcat managementHooks <> manageDocks
    , logHook = fadeWindowsLogHook fadeHook >> myLogHook xmproc
    }

theStartupHook :: X ()
theStartupHook = do
    xfork installDeps
    setWMName "LG3D"
    windows $ W.greedyView startupWorkspace
    spawnIfNotRunning term ""                 -- start terminal
    spawn "killall trayer"
    spawn "sleep 5; trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 2 --transparent true --alpha 0 --tint 0x222222 --height 16"
    spawn "xrandr --output HDMI2 --primary"
    spawn "xrandr --output HDMI2 --left-of eDP1"
    spawn "killall ibus-daemon"
    spawnIfNotRunning "nm-applet" ""
    spawn $ "feh --bg-scale " ++ background_img_path
    spawn "sudo powertop --auto-tune"
    spawnIfNotRunning "/usr/lib/notification-daemon-1.0/notification-daemon" "" -- libnotifiy
    spawnIfNotRunning "dropbox" ""
    spawn "setxkbmap -option caps:super"
    spawn "xmodmap ~/.xmodmap"
  where
    background_img_path = "~/.xmonad/images/background.*"

spawnIfNotRunning :: (MonadIO m) => String -> String -> m ()
spawnIfNotRunning cmd as =                    -- another hack
    spawn $ "if [ -z `pgrep " ++ cmd ++ " ` ] ; then " ++ cmd ++ " " ++ as ++ " ;fi"

main :: IO ()
main = do xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
          xmonad $ withUrgencyHook FocusHook $ conf xmproc -- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Hooks-UrgencyHook.html#v:focusUrgent
           -- move between workspaces with arrow keys
            `additionalKeys`  toList (planeKeys mod4Mask (Lines 1) Linear)
            `additionalKeysP` keybindings

--, ("M-f", sendKey noModMask xK_F11)
-- http://xmonad.org/xmonad-docs/X11/Graphics-X11-Types.html#t:KeySym
keybindings :: [(String, X ())]
keybindings =
    [ ("M-S-<Return>", spawn term)   -- M1 is actual alt (xmodmap)
    , ("M-x o", windows W.focusDown) -- also M-tab
    , ("M-x l", windows W.swapMaster >> windows W.focusDown)
    , ("M-s"
      , spawnSelected
            gsconfig
            [ ("term", term)
            , ("emacs", "xdotool key super+2 ; emacs")
            , ("firefox", "firefox")
            , ("chrome", "google-chrome")
            , ("spotify", "xdotool key super+4 ; spotify")
            , ("openmw", "openmw-launcher")])
    , ("M-x k", kill)
    , ("M-l", sendMessage Expand)  -- this is default
    , ("M-k", sendMessage Shrink)
    , ("M-i", sendMessage MirrorExpand)  -- resize vertically
    , ("M-j", sendMessage MirrorShrink)
    , ("M-w", spawn "~/.xmonad/toggle-wifi")
    , ("<F6>", spawn "xbacklight -time 500 -dec 10")
    , ("<F7>", spawn "xbacklight -time 500 -inc 10")
    , ("<F8>", spawn "~/.xmonad/toggle-mute")
    , ("<F9>", spawn $ "ponymix -d " ++ audioSink ++ " decrease 10")
    , ("<F10>", spawn $ "ponymix -d " ++ audioSink ++ " increase 10")
    , ("M-v", sendKey noModMask xK_Page_Down)
    , ("M-S-v", sendKey noModMask xK_Page_Up)
    , ("M-d", sendKey noModMask xK_Delete)
    , ("M-h", spawn "xterm -e \"/home/miles/Haskshell\"")
    , ("M-c", shellPrompt def)
    , ("C-M-l", spawn "pix")
    , ("M-a", nextScreen)
    , ("M-S-a", shiftNextScreen)
    , ("M-x x", defaultCommands >>= runCommand)
    ] ++
    fnMods
  where
    gsconfig =
      def
        { gs_colorizer = blackColorizer
        , gs_cellheight = 40
        , gs_cellwidth = 75
        }
    fnMods =
        [("M-<F" ++ show n ++ ">", sendKey noModMask $ fnKey n) | n <- [1 .. 10]]
    fnKey n = xK_F1 + n - 1 -- uses implementation of xK's as Word64's

-- ccw from bottom
blackColorizer :: HasColorizer a => a -> Bool -> X (String, String)
blackColorizer _ active =
  return $
  if active
     then ("white","black") -- background, text
     else ("black","white")

spawnSelected :: GSConfig String -> [(String, String)] -> X () --modified from GS source for full funtionality
spawnSelected gsc lst = gridselect gsc lst >>= flip whenJust spawn

audioSink :: String
audioSink = "alsa_output.pci-0000_00_1b.0.analog-stereo"

installDeps :: IO ()
installDeps = do
  oldAskpass <- lookupEnv "SUDO_ASKPASS"
  setEnv "SUDO_ASKPASS" "/usr/bin/ssh-askpass"
  mapM_ (\d -> isInstalled d >>= \b -> unless b $ void $ rawSystem "sudo" ["-A", "apt", "install", d]) deps
  unsetEnv "SUDO_ASKPASS"
  when (oldAskpass /= Nothing) $ setEnv "SUDO_ASKPASS" $ fromJust oldAskpass
    where deps = ["xbacklight", "feh", "trayer", "xmobar", "xrandr", "setxkbmap", "powertop", "htop", "xterm"]
          isInstalled :: String -> IO Bool
          isInstalled d = rawSystem "which" [d] >>= \code -> return $ code == ExitSuccess
