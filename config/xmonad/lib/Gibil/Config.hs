module Gibil.Config where

-- Custom Imports
import           Gibil.Colorschemes             (active)
import           Gibil.Layouts

-- XMonad imports
import           XMonad.Config
import           XMonad.Layout.Decoration.Theme


applyTheme ∷ Theme
applyTheme = def
    { activeColor = active color0
    , activeBorderColor = active color1
    , activeTextColor = inactive color2
    , decoHeight = 20
    , inactiveColor = active color10
    , inactiveBorderColor = active color3
    , inactiveTextColor = active color9
    , fontName = "xft:VictorMono Nerd Font:style=SemiBold"
    }

myConfig = def
    { modMask = mod4Mask
    , terminal = "kitty"
    , manageHook = namedScratchpadManageHook scratchpads
    , layoutHook = myLayoutHook
    , borderWidth = 2
    , logHook = updatePointer (0.5, 0.5) (0, 0)
                <> toggleFadeInactiveLogHook 0.9
                <> workspaceHistoryHook
                <> setWorkspaceNames
                <> logHook def
    , handleEventHook = followIfNoMagicFocus
                        <> minimizeEventHook
                        -- <> restartEventHook
                        <> myScratchPadEventHook
    , startupHook = myStartup
    , keys = customKeys (const []) addKeys
    }
