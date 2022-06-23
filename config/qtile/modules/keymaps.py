from libqtile.command import lazy
from libqtile.config import Click, Drag, Key

# More sophisticated key-names
alt = "mod1"
ctrl = "control"
mod = "mod4"
shift = "shift"
hyper = "mod3"

# Default applications
myTerminal = "kitty"
myBrowser = "firefox-devedition"

# Drag floating layouts.
mouse = [
    Drag(
        [mod],
        "Button1",
        lazy.window.set_position_floating(),
        start=lazy.window.get_position(),
    ),
    Drag(
        [mod], "Button2", lazy.window.set_size_floating(), start=lazy.window.get_size()
    ),
    Click([mod], "Button3", lazy.window.bring_to_front()),
]

window_navigation = [
    Key([mod], "h", lazy.layout.left()),
    Key([mod], "j", lazy.layout.down()),
    Key([mod], "k", lazy.layout.up()),
    Key([mod], "l", lazy.layout.right()),
]

window_displacement = [
    Key([mod], "Tab", lazy.layout.next()),  # Shift focus -> other window(s) in stack
    Key([mod], "Return", lazy.layout.swap_main()),
    Key([mod, shift], "h", lazy.layout.swap_left(), lazy.layout.shuffle_left()),
    Key([mod, shift], "j", lazy.layout.swap_down(), lazy.layout.shuffle_down()),
    Key([mod, shift], "k", lazy.layout.swap_up(), lazy.layout.shuffle_up()),
    Key([mod, shift], "l", lazy.layout.swap_right(), lazy.layout.shuffle_right()),
]

window_size_control = [
    Key([mod, ctrl], "h", lazy.layout.grow_left()),
    Key([mod, ctrl], "j", lazy.layout.grow_down()),
    Key([mod, ctrl], "k", lazy.layout.grow_up()),
    Key([mod, ctrl], "l", lazy.layout.grow_right()),
    Key([mod, ctrl], "n", lazy.layout.normalize()),  # Restore to original size
]

toggles = [
    Key([mod, shift], "c", lazy.window.kill()),
    Key([mod], "space", lazy.next_layout()),
    Key([mod], "t", lazy.window.toggle_floating()),
    Key([mod], "m", lazy.window.toggle_minimize()),
    Key([mod, shift], "m", lazy.window.toggle_maximize()),
    Key([mod, shift], "space", lazy.window.toggle_fullscreen()),
]

qtile_controls = [
    Key([mod, shift], "r", lazy.restart()),
    Key([mod, shift], "q", lazy.shutdown()),
]

rofi_spawns = [
    Key([mod], "p", lazy.spawn("rofi -show drun")),
    Key([mod], "b", lazy.spawn("rofi -show window")),
    Key([mod, alt], "p", lazy.spawn("rofi -show run")),
    Key([hyper], "p", lazy.spawn("rofi-systemd")),
]

application_spawns = [
    Key([mod, shift], "Return", lazy.spawn(myTerminal)),
    Key([mod, alt], "f", lazy.spawn(myBrowser)),
    Key(
        [mod, alt],
        "w",
        lazy.spawn(
            [myBrowser, "--profile ~/.mozilla/firefox/z5dgw9v6.dev-edition-private"]
        ),
    ),
    Key([mod, alt], "g", lazy.spawn("brave")),
    Key([mod, alt], "b", lazy.spawn([myTerminal, "-T Bottom -e btm"])),
]

audio_controls = [
    Key([], "XF86AudioMute", lazy.spawn("volctl --mute")),
    Key([], "XF86AudioRaiseVolume", lazy.spawn("volctl --up")),
    Key([], "XF86AudioLowerVolume", lazy.spawn("volctl --down")),
    Key([], "XF86AudioMicMute", lazy.spawn("micvol --mute")),
]

media_controls = [
    Key([mod], "Down", lazy.spawn("playerctl play-pause")),
    Key([mod], "Right", lazy.spawn("playerctl next")),
    Key([mod], "Left", lazy.spawn("playerctl previous")),
]

screenshot = [
    Key([], "Print", lazy.spawn("scrcap -w")),
    Key(["control"], "Print", lazy.spawn("scrcap -c -w")),
    Key([alt], "Print", lazy.spawn("scrcap -a")),
    Key([ctrl, alt], "Print", lazy.spawn("scrcap -c -a")),
    Key([shift], "Print", lazy.spawn("scrcap -r")),
    Key([ctrl, shift], "Print", lazy.spawn("scrcap -c -r")),
]

brightness_controls = [
    Key([], "XF86MonBrightnessUp", lazy.spawn("brightctl --up")),
    Key([], "XF86MonBrightnessDown", lazy.spawn("brightctl --down")),
]

quick_launch = [
    Key([], "XF86Calculator", lazy.spawn([myTerminal, "-T Qalc -e qalc"])),
]

keys = [
    *window_navigation,
    *window_displacement,
    *window_size_control,
    *toggles,
    *qtile_controls,
    *rofi_spawns,
    *application_spawns,
    *audio_controls,
    *media_controls,
    *screenshot,
    *brightness_controls,
    *quick_launch,
]
