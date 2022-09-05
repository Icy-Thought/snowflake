module Gibil.Keybindings where

-- Custom Modules
import qualified Gibil.Config                   as Cfg
import qualified Gibil.Scratchpads              as NS

-- General
import           XMonad

-- Mapping
import           XMonad.Actions.CopyWindow      (kill1)
import           XMonad.Actions.CycleWS         (toggleWS')
import           XMonad.Actions.OnScreen        (viewOnScreen)
import           XMonad.Actions.WithAll         (killAll)
import           XMonad.Layout.ResizableTile    (MirrorResize (..))
import           XMonad.Layout.SubLayouts       (GroupMsg (..), pullGroup)
import           XMonad.Layout.ToggleLayouts    (ToggleLayout (..))
import           XMonad.Layout.WindowNavigation (Direction2D (..))
import qualified XMonad.StackSet                as W
import           XMonad.StackSet                (sink)
import           XMonad.Util.EZConfig
import           XMonad.Util.Ungrab

myTerminal = String
myTerminal = "wezterm-gui"

myKeys ∷ [(String, X ())]
myKeys =
    [ ("M-S-z", spawn "xscreensaver-command -lock")
    , ("M-C-s", unGrab *> spawn "maim -s")
    -- Sublayout
    , ("M-C-\\", withFocused $ sendMessage . UnMergeAll)
    , ("M-C-h", sendMessage $ pullGroup L)
    , ("M-C-l", sendMessage $ pullGroup R)
    , ("M-C-k", sendMessage $ pullGroup U)
    , ("M-C-j", sendMessage $ pullGroup D)
    -- Scratchpads
    , ("M-A-s", NS.scratchpad NS.Calendar)
    , ("M-A-o", NS.scratchpad NS.SystemMonitor)
    , ("M-A-n", NS.scratchpad NS.Notes)
    , ("M-A-t", NS.scratchpad NS.Terminal)
    ]

mousebindings =
  [ ((C.modKey .|. mod1Mask, button1), \w -> focus w >> mouseResizeWindow w)
  ]
