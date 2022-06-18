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
        [mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()
    ),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

keys = [
    # Switch between windows in current stack pane
    Key([mod], "h", lazy.layout.right()),
    Key([mod], "j", lazy.layout.down()),
    Key([mod], "k", lazy.layout.up()),
    Key([mod], "l", lazy.layout.left()),
    # Move windows up or down in current stack
    Key([mod, shift], "h", lazy.layout.swap_right(), lazy.layout.shuffle_right()),
    Key([mod, shift], "j", lazy.layout.swap_down(), lazy.layout.shuffle_down()),
    Key([mod, shift], "k", lazy.layout.swap_up(), lazy.layout.shuffle_up()),
    Key([mod, shift], "l", lazy.layout.swap_left(), lazy.layout.shuffle_left()),
    # Switch window focus to other pane(s) of stack
    Key([mod], "tab", lazy.layout.next()),
    # Swap panes of split stack
    Key([mod, shift], "space", lazy.layout.rotate()),
    # Toggle between split and unsplit sides of stack.
    Key([mod, shift], "Return", lazy.layout.toggle_split()),
    # System-dependent application spawns
    Key([mod], "p", lazy.spawn("rofi -show drun")),
    Key([mod], "b", lazy.spawn("rofi -show window")),
    Key([mod, alt], "p", lazy.spawn("rofi -show run")),
    Key([hyper], "p", lazy.spawn("rofi-systemd")),
    # Pre-defined application spawns
    Key([mod, shift], "Return", lazy.spawn(myTerminal)),
    Key([mod, alt], "f", lazy.spawn(myBrowser)),
    Key(
        [mod, alt],
        "w",
        lazy.spawn(
            [myBrowser, "--profile ~/.mozilla/firefox/z5dgw9v6.dev-edition-private"]
        ),
    ),
    Key([mod, shift], "g", lazy.spawn("brave")),
    Key([mod, shift], "b", lazy.spawn([myTerminal, "-T Bottom -e btm"])),
    # Toggle between different layouts as defined below
    Key([mod], "space", lazy.next_layout()),
    Key([mod, shift], "c", lazy.window.kill()),
    Key([mod, shift], "q", lazy.shutdown()),
    # Volume Controls
    Key([], "XF86AudioMute", lazy.spawn("volctl --mute")),
    Key([], "XF86AudioRaiseVolume", lazy.spawn("volctl --up")),
    Key([], "XF86AudioLowerVolume", lazy.spawn("volctl --down")),
    Key([], "xF86XK_AudioMicMute", lazy.spawn("micvol --mute")),
    # Media Controls
    Key([mod], "Down", lazy.spawn("playerctl play-pause")),
    Key([mod], "Right", lazy.spawn("playerctl next")),
    Key([mod], "Left", lazy.spawn("playerctl previous")),
    # Brightness Controls
    Key([], "xF86XK_MonBrightnessUp", lazy.spawn("brightctl --up")),
    Key([], "xF86XK_MonBrightnessDown", lazy.spawn("brightctl --down")),
    # Screenshot Controls
    Key([], "Print", lazy.spawn("scrcap -w")),
    Key(["control"], "Print", lazy.spawn("scrcap -c -w")),
    Key([alt], "Print", lazy.spawn("scrcap -a")),
    Key([ctrl, alt], "Print", lazy.spawn("scrcap -c -a")),
    Key([shift], "Print", lazy.spawn("scrcap -r")),
    Key([ctrl, shift], "Print", lazy.spawn("scrcap -c -r")),
]
