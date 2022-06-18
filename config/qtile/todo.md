# XMonad-like Setup
1. Keybindings -> check github..
    a. https://github.com/Icy-Thought/Snowflake/blob/1641e041d3afe43979afe9d43213427d856d9a39/config/my-xmonad/xmonad.hs#L973

## Layout (4):
b. large main
d. tabbed

related info:
```haskell 
  |||! rename "Large Main" (Tall 1 (3 / 100) (3 / 4))
  |||! myTabbed
where myTabbed = rename "Tabbed" $ tabbed shrinkText icyTheme
```

## Toggled spawns (rofi):
a. MagicFocus (view window -> main window)
b. Magnify (view window => grows larger)

## Spawns: 
e. `mod+alt+space` = tabbed layout

### `mod+alt` spawns:
a. `m` = "brave --new-window https://mail.proton.me/u/0/inbox"

## Aesthetics
5. Specify font, if possible done through nix. (XResources??)
6. Space between windows (gaps) = 20

## Interesting features: 
c. Tile (mod+t), floating (mod+drag)
d. Minimize and maximize windows on desire

# Keymaps
```python
Key([mod, alt], "b", lazy.spawn("rofi -show window")), TODO: swap with X window
Key([mod], "g", lazy.spawn("rofi -show window")), TODO: go to X window
```
