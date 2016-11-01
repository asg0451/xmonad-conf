-- forked from XMonad.Actions.Commands
-----------------------------------------------------------------------------
-- |
-- Module      :  Commands
-- Copyright   :  (c) David Glasser 2007, Miles Frankel 2016
-- License     :  BSD3
--
-- Maintainer  :  miles.frankel@gmail.com
-- Stability   :  stable
-- Portability :  portable
--
-- Allows you to run internal xmonad commands (X () actions) using
-- a dmenu menu in addition to key bindings.
-----------------------------------------------------------------------------

module Command
       (commandMap, runCommand, workspaceCommands, screenCommands,
        defaultCommands)
       where

import           XMonad
import           XMonad.StackSet           hiding (workspaces)

import           Control.Monad
import qualified Data.Map                  as M
import           Data.Maybe
import           System.Exit
import           XMonad.Actions.GridSelect
-- $usage
--
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- >    import XMonad.Actions.Commands
--
-- Then add a keybinding to the runCommand action:
--
-- >    , ((modm .|. controlMask, xK_y), commands >>= runCommand)
--
-- and define the list of commands you want to use:
--
-- >    commands :: X [(String, X ())]
-- >    commands = defaultCommands
--
-- Whatever key you bound to will now cause a popup menu of internal
-- xmonad commands to appear.  You can change the commands by changing
-- the contents of the list returned by 'commands'.  (If you like it
-- enough, you may even want to get rid of many of your other key
-- bindings!)
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".

-- | Create a 'Data.Map.Map' from @String@s to xmonad actions from a
--   list of pairs.
commandMap :: [(String, X ())] -> M.Map String (X ())
commandMap c = M.fromList c

-- | Generate a list of commands to switch to\/send windows to workspaces.
workspaceCommands :: X [(String, X ())]
workspaceCommands = asks (workspaces . config) >>= \spaces -> return
                            [((m ++ show i), windows $ f i)
                                | i <- spaces
                                , (f, m) <- [(view, "view"), (shift, "shift")] ]

-- | Generate a list of commands dealing with multiple screens.
screenCommands :: [(String, X ())]
screenCommands = [((m ++ show sc), screenWorkspace (fromIntegral sc) >>= flip whenJust (windows . f))
                      | sc <- [0, 1]::[Int] -- TODO: adapt to screen changes
                      , (f, m) <- [(view, "screen"), (shift, "screen-to-")]
                 ]

-- | A nice pre-defined list of commands.
defaultCommands :: X [(String, X ())]
defaultCommands = do
    wscmds <- workspaceCommands
    return $ wscmds ++ screenCommands ++ otherCommands
 where
    otherCommands =
        [ ("shrink"              , sendMessage Shrink                               )
        , ("expand"              , sendMessage Expand                               )
        , ("next-layout"         , sendMessage NextLayout                           )
        , ("default-layout"      , asks (layoutHook . config) >>= setLayout         )
        , ("restart-wm"          , restart "xmonad" True                            )
        , ("restart-wm-no-resume", restart "xmonad" False                           )
        , ("xterm"               , spawn =<< asks (terminal . config)               )
        , ("kill"                , kill                                             )
        , ("refresh"             , refresh                                          )
        , ("focus-up"            , windows focusUp                                  )
        , ("focus-down"          , windows focusDown                                )
        , ("swap-up"             , windows swapUp                                   )
        , ("swap-down"           , windows swapDown                                 )
        , ("swap-master"         , windows swapMaster                               )
        , ("sink"                , withFocused $ windows . sink                     )
        , ("quit-wm"             , io $ exitWith ExitSuccess                        )
        ]

-- | Given a list of command\/action pairs, prompt the user to choose a
--   command and return the corresponding action.
runCommand :: [(String, X ())] -> X ()
runCommand commands = do
  selection <- gridselect def commands
  when (not $ isNothing selection) $ do
    fromJust selection
