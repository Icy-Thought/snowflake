from libqtile.command import lazy
from libqtile.config import EzClick, EzDrag, EzKey

# Default applications
myTerminal = "kitty"
myBrowser = "firefox-devedition"

EzKey.modifier_keys = {
    "M": "mod4",
    "A": "mod1",
    "S": "shift",
    "C": "control",
    "H": "mod3",
}

# Drag floating layouts.
mouse = [
    EzDrag(
        "M-<Button1>",
        lazy.window.set_position_floating(),
        start=lazy.window.get_position(),
    ),
    EzDrag(
        "M-<Button2>", lazy.window.set_size_floating(), start=lazy.window.get_size()
    ),
    EzClick("M-<Button3>", lazy.window.bring_to_front()),
]

window_navigation = [
    EzKey("M-h", lazy.layout.left()),
    EzKey("M-j", lazy.layout.down()),
    EzKey("M-k", lazy.layout.up()),
    EzKey("M-l", lazy.layout.right()),
]

window_displacement = [
    EzKey("M-<Tab>", lazy.layout.next()),  # Shift focus -> other window(s) in stack
    EzKey("M-<Return>", lazy.layout.swap_main()),
    EzKey("M-S-h", lazy.layout.swap_left(), lazy.layout.shuffle_left()),
    EzKey("M-S-j", lazy.layout.swap_down(), lazy.layout.shuffle_down()),
    EzKey("M-S-k", lazy.layout.swap_up(), lazy.layout.shuffle_up()),
    EzKey("M-S-l", lazy.layout.swap_right(), lazy.layout.shuffle_right()),
]

window_size_control = [
    EzKey("M-C-h", lazy.layout.grow_left()),
    EzKey("M-C-j", lazy.layout.grow_down()),
    EzKey("M-C-k", lazy.layout.grow_up()),
    EzKey("M-C-l", lazy.layout.grow_right()),
    EzKey("M-C-n", lazy.layout.normalize()),  # Restore to original size
]

toggles = [
    EzKey("M-S-c", lazy.window.kill()),
    EzKey("M-<space>", lazy.next_layout()),
    EzKey("M-t", lazy.window.toggle_floating()),
    EzKey("M-m", lazy.window.toggle_minimize()),
    EzKey("M-S-<space>", lazy.window.toggle_maximize()),
    EzKey("M-C-<space>", lazy.window.toggle_fullscreen()),
]

qtile_controls = [
    EzKey("M-S-r", lazy.restart()),
    EzKey("M-S-q", lazy.shutdown()),
]

rofi_spawns = [
    EzKey("M-p", lazy.spawn("rofi -show drun")),
    EzKey("M-b", lazy.spawn("rofi -show window")),
    EzKey("M-A-p", lazy.spawn("rofi -show run")),
    EzKey("H-p", lazy.spawn("rofi-systemd")),
]

application_spawns = [
    EzKey("M-S-<Return>", lazy.spawn(myTerminal)),
    EzKey("M-A-f", lazy.spawn(myBrowser)),
    EzKey(
        "M-A-w",
        lazy.spawn(
            [myBrowser, "--profile ~/.mozilla/firefox/z5dgw9v6.dev-edition-private"]
        ),
    ),
    EzKey("M-A-g", lazy.spawn("brave")),
    EzKey("M-A-b", lazy.spawn([myTerminal, "-T Bottom -e btm"])),
]

audio_controls = [
    EzKey("<XF86AudioMute>", lazy.spawn("volctl --mute")),
    EzKey("<XF86AudioRaiseVolume>", lazy.spawn("volctl --up")),
    EzKey("<XF86AudioLowerVolume>", lazy.spawn("volctl --down")),
    EzKey("<XF86AudioMicMute>", lazy.spawn("micvol --mute")),
]

media_controls = [
    EzKey("M-<Down>", lazy.spawn("playerctl play-pause")),
    EzKey("M-<Right>", lazy.spawn("playerctl next")),
    EzKey("M-<Left>", lazy.spawn("playerctl previous")),
]

screenshot = [
    EzKey("<Print>", lazy.spawn("scrcap -w")),
    EzKey("C-<Print>", lazy.spawn("scrcap -c -w")),
    EzKey("A-<Print>", lazy.spawn("scrcap -a")),
    EzKey("C-A-<Print>", lazy.spawn("scrcap -c -a")),
    EzKey("S-<Print>", lazy.spawn("scrcap -r")),
    EzKey("C-S-<Print>", lazy.spawn("scrcap -c -r")),
]

brightness_controls = [
    EzKey("<XF86MonBrightnessUp>", lazy.spawn("brightctl --up")),
    EzKey("<XF86MonBrightnessDown>", lazy.spawn("brightctl --down")),
]

quick_launch = [
    EzKey("<XF86Calculator>", lazy.spawn([myTerminal, "-T Qalc -e qalc"])),
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
