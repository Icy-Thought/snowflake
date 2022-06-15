# XMonad-like Setup
1. Keybindings -> check github..
    a. https://github.com/Icy-Thought/Snowflake/blob/1641e041d3afe43979afe9d43213427d856d9a39/config/my-xmonad/xmonad.hs#L973

## Layout (4):
a. 2 columns
b. large main
c. 3 columns
d. tabbed

related info:
```haskell 
layoutsStart (rename "2 Columns"  (Tall 1 (3 / 100) (1 / 2)))
  |||! rename "Large Main" (Tall 1 (3 / 100) (3 / 4))
  |||! rename "3 Columns"  (multiCol [1, 1] 2 0.01 (-0.5))
  |||! myTabbed
where myTabbed = rename "Tabbed" $ tabbed shrinkText icyTheme
```

## Toggled spawns (rofi):
a. MagicFocus (view window -> main window)
b. Magnify (view window => grows larger)

## Spawns: 
a. `mod+enter` = rofi 
b. `mod+shift+enter` = rofi-commands 
c. `hyper + p` = rofi-systemd
e. `mod+enter` = tabbed layout

### `mod+alt` spawns:
a. `m` = "brave --new-window https://mail.proton.me/u/0/inbox"
b. `w` = "firefox-devedition --profile ~/.mozilla/firefox/z5dgw9v6.dev-edition-private"
c. `d` = discord

## Aesthetics
5. Specify font, if possible done through nix. (XResources??)
6. Space between windows (gaps) = 20

### Borders:
a. colorscheme based.
b. width = 2

## Interesting features: 
a. Bring window (mod+b)
b. bring class (mod+shift+b)
c. Tile (mod+t), floating (mod+drag)
d. Minimize and maximize windows on desire
e. Directional navigation = vim (h,j,k,l)

## Built-in status-bar? 
Interesting...

## Scratchpads:
a. browser, when title is either "Proton Mail" or "@proton.me".

### Sizing:
```haskell
nearFullFloat = customFloating $ W.RationalRect l t w h
 where
  h = 0.9
  w = 0.9
  t = 0.95 - h
  l = 0.95 - w
```

```python
def maximum_float(title: str, cmd: str) -> str:
    return (title, cmd,
            x=0.05, y=0.05, width=0.9, height=0.9, opacity=1.0,
            on_focus_lost_hide=True)

groups = [
    Scratchpad("scratchpad", [
        DropDown(maximum_float("Qtile Shell", "kitty -T Bottom -e btm"))
    ])
    Group("a"),
]
```

### `NS`-spawns
```haskell
scratchpads =
  [ NS "bottom"             "kitty -T Bottom -e btm"    title =? "Bottom"               nearFullFloat
  , NS "discord"            "discord"                   className =? "discord"          nearFullFloat
  , NS "emacs"              "emacsclient -c"            className =? "emacs"            nonFloating
  , NS "neovide"            "neovide"                   className =? "neovide"          nearFullFloat
  , NS "Picture-in-Picture" "Picture-in-Picture"        title =? "Picture-in-Picture"   defaultFloating
  , NS "protonmail"         protonMailCommand           protonMailSelector              nearFullFloat
  , NS "spotify"            "spotify"                   className =? "Spotify"          nearFullFloat
  , NS "transmission"       "transmission-gtk"          className =? "Transmission-gtk" nearFullFloat
  ]
```
