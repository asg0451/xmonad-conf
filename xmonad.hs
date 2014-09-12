import XMonad
import XMonad.Hooks.SetWMName
import XMonad.Layout.Grid
import XMonad.Layout.ResizableTile
import XMonad.Layout.IM
import XMonad.Layout.ThreeColumns
import XMonad.Layout.NoBorders
import XMonad.Layout.Circle
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Fullscreen
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog
import XMonad.Actions.Plane
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ICCCMFocus
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Data.Ratio ((%))

import XMonad.Actions.GridSelect
import Data.Word (Word8)
import Text.Printf

{-
  Xmonad configuration variables. These settings control some of the
  simpler parts of xmonad's behavior and are straightforward to tweak.
-}

myFocusedBorderColor = "#ff0000"      -- color of focused border
myNormalBorderColor  = "#cccccc"      -- color of inactive border
myBorderWidth        = 1              -- width of border around windows
myTerminal           = "xfce4-terminal"   -- which terminal software to use


{-
  Xmobar configuration variables. These settings control the appearance
  of text which xmonad is sending to xmobar via the DynamicLog hook.
-}

myTitleColor     = "#eeeeee"  -- color of window title
myTitleLength    = 80         -- truncate window title to this length
myCurrentWSColor = "#e6744c"  -- color of active workspace
myVisibleWSColor = "#c185a7"  -- color of inactive workspace
myUrgentWSColor  = "#cc0000"  -- color of workspace with 'urgent' window
myCurrentWSLeft  = "["        -- wrap active workspace with these
myCurrentWSRight = "]"
myVisibleWSLeft  = "("        -- wrap inactive workspace with these
myVisibleWSRight = ")"
myUrgentWSLeft  = "{"         -- wrap urgent workspace with these
myUrgentWSRight = "}"


myWorkspaces =
  [
    "1:Term","2:Emacs","3:Web","4:Files", "5:Misc"
  ]

startupWorkspace = "1:Term"



defaultLayouts =
    smartBorders(avoidStruts(
                             ResizableTall 1 (3/100) (1/2) []
                             ||| Mirror (ResizableTall 1 (3/100) (1/2) [])
                             ||| noBorders Full
                             ||| ThreeColMid 1 (3/100) (3/4)
                             ||| Circle
                             ||| Grid))

myLayouts =
-- onWorkspace "9:Pix" gimpLayout $
   defaultLayouts

{-
  xprop
    - WM_CLASS(STRING): values in this list of strings can be compared
      to "className" to match windows.
    - WM_NAME(STRING): this value can be compared to "resource" to match
      windows.
    - doIgnore: this tells xmonad to completely ignore the window. It will
      not be tiled or floated. Useful for things like launchers and
      trays.
    - doFloat: this tells xmonad to float the window rather than tiling
      it. Handy for things that pop up, take some input, and then go away,
      such as dialogs, calculators, and so on.
    - doF (W.shift "Workspace"): this tells xmonad that when this program
      is launched it should be sent to a specific workspace.
-}

myManagementHooks :: [ManageHook]
myManagementHooks =
    [ className =? "Emacs"           --> doF (W.shift "2:Emacs")
    , className =? "xfce4-terminal"  --> doF (W.shift "1:Term")
    , className =? "Midori"          --> doF (W.shift "3:Web")
    , className =? "Thunar"          --> doF (W.shift "4:Files")
    , className =? "XTerm"           --> doFloat
    ]


{-
  Here we actually stitch together all the configuration settings
  and run xmonad. We also spawn an instance of xmobar and pipe
  content into it via the logHook.
-}

main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"

  dzenLeftBar <- spawnPipe ""

  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig {
    focusedBorderColor = myFocusedBorderColor
  , normalBorderColor = myNormalBorderColor
  , terminal = myTerminal
  , borderWidth = myBorderWidth
  , layoutHook = myLayouts
  , workspaces = myWorkspaces
  , modMask = mod4Mask
  , handleEventHook = fullscreenEventHook <+> docksEventHook -- added <+>..
  , startupHook = do
      setWMName "LG3D"
      windows $ W.greedyView startupWorkspace
      spawn "~/.xmonad/startup-hook"
  , manageHook = manageHook defaultConfig
      <+> composeAll myManagementHooks
      <+> manageDocks
  , logHook = takeTopFocus <+> dynamicLogWithPP xmobarPP {
      ppOutput = hPutStrLn xmproc
      , ppTitle = xmobarColor myTitleColor "" . shorten myTitleLength
      , ppCurrent = xmobarColor myCurrentWSColor ""
        . wrap myCurrentWSLeft myCurrentWSRight
      , ppVisible = xmobarColor myVisibleWSColor ""
        . wrap myVisibleWSLeft myVisibleWSRight
      , ppUrgent = xmobarColor myUrgentWSColor ""
        . wrap myUrgentWSLeft myUrgentWSRight
    }
  }  `additionalKeysP` [   -- M-space is cycle layouts. M-S-space reset to default layout
                         ("M-S-<Return>", spawn "xfce4-terminal")
                       , ("M-x o", windows W.focusDown)
                       , ("M-s", spawnSelected defaultGSConfig
                                 [ myTerminal,"midori&","thunar&","emacs&"])
                       , ("M-x k", kill)
                       , ("M-p", sendMessage Expand)
                       , ("M-m", sendMessage Shrink)
                       ]                           -- M1 is actual alt (xmodmap)

  {-`removeKeysP` [
                 ("M-S-c")
                ]
-}
