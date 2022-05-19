<img alt="WM-related Configurations" src="../.assets/main/wm-related.png" align="center">

# Introduction
The following folder contains several WM's tried throughout the years of my NixOS usage. Hopefully you'll find something that attracts you to NixOS here and start using it.

# Table of Contents
- [Introduction](#introduction)
- [XMonad](#xmonad)
  - [Keybindings](#keybindings)
- [Gnome](#gnome)
- [Special Thanks](#special-thanks)

# XMonad
<img alt="XMonad Desktop" src="../.assets/main/xmonad-desktop.png" align="center">

## Keybindings
*Credits:* [micrub](https://gist.github.com)/[xmonad-default-key-bindings.md](https://gist.github.com/micrub/aeebe7eb4d2df9e5e203e76a0fd89542)

Definitions:
- <kbd>modm</kbd> = <kbd>super</kbd> (windows key)
- <kbd>modalt</kbd> = <kbd>super</kbd> + <kbd>alt</kbd>
- <kbd>hyper</kbd> = <kbd>super</kbd> + <kbd>ctrl</kbd> + <kbd>shift</kbd>

### Action Keybindings

| Key binding                                            | Action                                                                |
|--------------------------------------------------------|-----------------------------------------------------------------------|
| <kbd>modm</kbd> + <kbd>shift</kbd> + <kbd>/</kbd>      | Run xmessage with a summary of the default keybindings.               |
| <kbd>modm</kbd> + <kbd>shift</kbd> + <kbd>return</kbd> | Launch terminal. (Alacritty)                                          |
| <kbd>modm</kbd> + <kbd>alt</kbd> + <kbd>f</kbd>        | Launch Firefox Browser.                                               |
| <kbd>modm</kbd> + <kbd>alt</kbd> + <kbd>h</kbd>        | Launch htop.                                                          |
| <kbd>modm</kbd> + <kbd>alt</kbd> + <kbd>s</kbd>        | Launch Spotify.                                                       |
| <kbd>modm</kbd> + <kbd>p</kbd>                         | Invoke rofi to select an application specified in a desktop file.     |
| <kbd>modm</kbd> + <kbd>g</kbd>                         | Invoke rofi to select which window the focus will be shifted towards. |
| <kbd>modm</kbd> + <kbd>b</kbd>                         | Invoke rofi to bring *X* window to current workspace.                 |
| <kbd>modm</kbd> + <kbd>shift</kbd> + <kbd>b</kbd>      | Invoke rofi to select a window to replace the active window with.     |
| <kbd>hyper</kbd> + <kbd>l</kbd>                        | Invoke rofi to select from available layouts.                         |
| <kbd>modm</kbd> + <kbd>shift</kbd> + <kbd>c</kbd>      | Close focused window.                                                 |
| <kbd>modm</kbd> + <kbd>q</kbd>                         | Restart XMonad.                                                       |
| <kbd>modm</kbd> + <kbd>shift</kbd> + <kbd>q</kbd>      | Quit XMonad.                                                          |

### Window Movement Keybindings

| Key binding                                               | Action                                                                           |
|-----------------------------------------------------------|----------------------------------------------------------------------------------|
| <kbd>modm</kbd> + <kbd>space</kbd>                        | Cycle through available layouts.                                                 |
| <kbd>modm</kbd> + <kbd>shift</kbd> + <kbd>space</kbd>     | Reset current workspace layouts to default.                                      |
| <kbd>modm</kbd> + <kbd>n</kbd>                            | Resize viewed windows to default size.                                           |
| <kbd>modm</kbd> + <kbd>tab</kbd>                          | Move focus to the next window.                                                   |
| <kbd>modm</kbd> + <kbd>shift</kbd> + <kbd>tab</kbd>       | Move focus to the previous window.                                               |
| <kbd>modm</kbd> + <kbd>{h,j,k,l}</kbd>                    | Move to an adjacent window in the direction associated with the pressed vim-key. |
| <kbd>modm</kbd> + <kbd>m</kbd>                            | Minimize active window.                                                          |
| <kbd>modm</kbd> + <kbd>shift</kbd> + <kbd>m</kbd>         | Maximize active window.                                                          |
| <kbd>modm</kbd> + <kbd>return</kbd>                       | Swap focused window with master window.                                          |
| <kbd>modm</kbd> + <kbd>shift</kbd> + <kbd>{h,j,k,l}</kbd> | Swap focused window with window in the input direction.                          |
| <kbd>modm</kbd> + <kbd>comma</kbd>                        | Increment number of windows in master area.                                      |
| <kbd>modm</kbd> + <kbd>period</kbd>                       | Deincrement number of windows in master area.                                    |
| <kbd>modm</kbd> + <kbd>shift</kbd> + <kbd>z</kbd>         | Move active window to an empty workspace.                                        |
| <kbd>modm</kbd> + <kbd>shift</kbd> + <kbd>h</kbd>         | Move active window to an empty workspace + change to that workspace.             |
| <kbd>modm</kbd> + <kbd>ctrl</kbd> + <kbd>space</kbd>      | Activates/Deactivates fullscreen for active window.                              |
| <kbd>modm</kbd> + <kbd>x</kbd>                            | Sends active window to hidden workspace.                                         |
| <kbd>modm</kbd> + <kbd>alt</kbd> + <kbd>space</kbd>       | Minimizes other classes but the active window class.                             |
| <kbd>hyper</kbd> + <kbd>g</kbd>                           | Gather class of active window to current workspace.                              |

### Window Movement Mouse Keybindings

| Binding                              | Action                                       |
|--------------------------------------|----------------------------------------------|
| <kbd>modm</kbd> + <kbd>button1</kbd> | Window -> floating mode, move by dragging.   |
| <kbd>modm</kbd> + <kbd>button3</kbd> | Window -> floating mode, resize by dragging. |
| <kbd>modm</kbd> + <kbd>button2</kbd> | Raise window to top of the stack.            |

### Workspace Movement Keybindings

| Key binding                                            | Action                                    |
|--------------------------------------------------------|-------------------------------------------|
| <kbd>modm</kbd> + <kbd>[1..9]</kbd>                    | Switch to n-workspace.                    |
| <kbd>modm</kbd> + <kbd>shift</kbd> + <kbd>[1..9]</kbd> | Move client to n-workspace.               |
| <kbd>hyper</kbd> + <kbd>e                              | Move to empty workspace.                  |
| <kbd>modm</kbd> + <kbd>\\</kbd>                        | Cycles through current workspace history. |

### Screen Movement Keybindings

| Key binding                                             | Action                                          |
|---------------------------------------------------------|-------------------------------------------------|
| <kbd>modm</kbd> + <kbd>{w,e,r}</kbd>                    | Switch to physical/Xinerama screens 1, 2, or 3. |
| <kbd>modm</kbd> + <kbd>shift</kbd> + <kbd>{w,e,r}</kbd> | Move client to screen 1, 2, or 3.               |

# Gnome
<img alt="Gnome Desktop" src="../.assets/main/gnome-desktop.png" align="center">

# Special Thanks 🪔
- [IvanMalison](https://github.com/IvanMalison): XMonad Setup.
