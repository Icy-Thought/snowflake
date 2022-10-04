module Gibil.Colorscheme where

import           XMonad.Layout.Decoration.Theme

data Colorscheme = Colorscheme
  { color0  :: String
  , color1  :: String
  , color2  :: String
  , color3  :: String
  , color4  :: String
  , color5  :: String
  , color6  :: String
  , color7  :: String
  , color8  :: String
  , color9  :: String
  , color10 :: String
  }
  deriving (Eq, Show, Read)

catppuccin :: Colorscheme
catppuccin = Colorscheme { color0  = "#d8dee9"
                         , color1  = "#1e1e2e"
                         , color2  = "#313244"
                         , color3  = "#f5a97f"
                         , color4  = "#f5c2e7"
                         , color5  = "#f2cdcd"
                         , color6  = "#eba0ac"
                         , color7  = "#f2779c"
                         , color8  = "#c9cbff"
                         , color9  = "#b5e8e0"
                         , color10 = "#b1e1a6"
                         }

-- FIXME
tokyoNight :: Colorscheme
tokyoNight = Colorscheme { color0  = "#c0caf5"
                         , color1  = "#cfc9c2"
                         , color2  = "#9d7cd8"
                         , color3  = "#b4f9f8"
                         , color4  = "#9ece6a"
                         , color5  = "#f7768e"
                         , color6  = "#ff9e64"
                         , color7  = "#e0af68"
                         , color8  = "#bb9af7"
                         , color9  = "#7dcfff"
                         , color10 = "#565f89"
                         }

active = tokyoNight

applyTheme :: Theme
applyTheme = def { activeColor         = active color0
                 , activeBorderColor   = active color1
                 , activeTextColor     = inactive color2
                 , decoHeight          = 20
                 , inactiveColor       = active color10
                 , inactiveBorderColor = active color3
                 , inactiveTextColor   = active color9
                 , fontName = "xft:VictorMono Nerd Font:style=SemiBold"
                 }
