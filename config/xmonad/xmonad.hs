module Main where

-- Haskell Imports
import           Control.Monad
import           Data.List
import qualified Data.Map                   as M
import           Data.Maybe
import           Data.Monoid
-- Custom Imports
import qualified Gibil.Keybindings
import           Gibil.Layouts
import           Gibil.Scratchpads          (scratchpads)
import           Gibil.Theme                (active)
-- XMonad Imports
import           XMonad
import           XMonad.Config
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageHelpers

main :: IO ()
main = xmonad . ewmhFullscreen . ewmh $ myConfig

myConfig :: XConfig
myConfig = def
  { modMask         = mod4Mask
  , terminal        = "alacritty"
  , manageHook      = myManageHook <> namedScratchpadManageHook scratchpads
  , layoutHook      = myLayoutHook
  , borderWidth     = 2
  , logHook         = updatePointer (0.5, 0.5) (0, 0)
                      <> toggleFadeInactiveLogHook 0.9
                      <> workspaceHistoryHook
                      <> setWorkspaceNames
                      <> logHook def
  , handleEventHook = followIfNoMagicFocus
                      <> minimizeEventHook
                      <> restartEventHook
                      <> myScratchPadEventHook
  , startupHook     = myStartup
  , keys            = customKeys (const []) addKeys
  }

myManageHook :: ManageHook
myManageHook = composeAll
  [ isFullscreen --> doFullFloat
  , isDialog --> doFloat
  , className =? "MPlayer" --> doFloat
  , className =? "Gimp" --> doFloat
  , resource =? "desktop_window" --> doIgnore
  ]
