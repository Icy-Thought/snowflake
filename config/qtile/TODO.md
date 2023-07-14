# Qtile
## Status-bar (qtile-extras)
- [ ] `UPowerWidget`: better battery <- icon + bg change on low level
- [ ] `CurrentLayoutIcon`: working foreground color with `use_mask = True`
- [ ] (Maybe) `BrightnessControl`: visual + clickable control. <- use only if
      layout can change.

# XMonad-like Setup
## Toggled spawns (rofi):
- [ ] MagicFocus (view window -> main window)
- [ ] Magnify (view window => grows larger)

### `mod+alt` spawns:
- [ ] `m` = "ungoogled-chromium --new-window https://mail.proton.me/u/0/inbox" <- floating
    - Make sure it floats for both firefox and ungoogled-chromium.

```haskell
isProtonMailTitle t = isInfixOf "@proton.me" t && isInfixOf "Proton Mail" t
```

## Aesthetics
- [ ] Fix statusbar icon paths + apply whitsur icon theme to qtile.. seems to
      not follow theme specifications from `modules.themes`.. 
- [ ] Have bar show active applications inside workspaces, like taffybar
    - Or, like Chromium, show what applicatiosn are in a certain workspace on
    hover

# Functionality
- [ ] When workspace has one active window -> toggle.maximum(), otherwise -> disabled.
    - Ability to cycle + switch focus on fullscreen to other application.
- [ ] Add rofi power spawn on battery widget click.
- [ ] Tray == missing lanched applications????
- [ ] ungoogled-chromium seems to lose focus on tab kill???

# Keymaps
```python
EzKey(["M-A-b", lazy.spawn("rofi -show window")), TODO: swap with X window
EzKey("M-g", lazy.spawn("rofi -show window")), TODO: go to X window
```

Migrate to Qtile
```haskell
[ ((modm, xK_g)                     , myGoToWindow)
, ((modm .|. shiftMask, xK_b)       , myReplaceWindow)
, ((modm .|. controlMask, xK_space) , deactivateFullOr goFullscreen)
, ((modm, xK_x), addHiddenWorkspace "NSP" >> windows (W.shift "NSP"))
, ((modalt, xK_space), deactivateFullOr restoreOrMinimizeOtherClasses)
, ((hyper, xK_g)                    , gatherThisClass)
,
-- Focus/layout manipulation
  ((modm, xK_e)       , goToNextScreenX)
, ((modm, xK_slash)   , sendMessage $ Toggle MIRROR)
, ( (modm, xK_backslash)
  , cycleWorkspaceOnCurrentScreen [xK_Super_L] xK_backslash xK_slash
  )
, ((modm, xK_z)                 , shiftToNextScreenX)
, ((modm .|. shiftMask, xK_z)   , shiftToEmptyNextScreen)
, ((modm .|. shiftMask, xK_h)   , shiftToEmptyAndView)
, ((hyper, xK_5), getWorkspaceDmenu >>= windows . SW.swapWithCurrent)
,
-- These ought to be rebound for boringWindows support
  ((hyper, xK_e)                , moveTo Next emptyWS)
,
-- Miscellaneous XMonad
  ((hyper, xK_1)                , toggleFadingForActiveWindow)
, ((hyper .|. shiftMask, xK_1)  , toggleFadingForActiveWorkspace)
, ((hyper .|. controlMask, xK_1), toggleFadingForActiveScreen)
, ((hyper, xK_t)                , selectToggle)
, ((modalt, xK_4)               , selectLimit)
, ((hyper, xK_3)                , addWorkspacePrompt def)
, ((modalt, xK_3)               , selectWorkspace def)
, ((hyper .|. mod1Mask, xK_3)   , removeWorkspace)
, ((hyper, xK_l)                , selectLayout)
```
