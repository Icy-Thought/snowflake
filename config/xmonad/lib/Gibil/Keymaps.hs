module Gibil.Keymaps where

-- Modules
import           XMonad.Util.EZConfig
import           XMonad.Util.Ungrab

myTerminal = String
myTerminal = "wezterm-gui"

myKeys ∷ [(String, X ())]
myKeys =
    [ ("M-S-z", spawn "xscreensaver-command -lock")
    , ("M-C-s", unGrab *> spawn "maim -s")
    ]
