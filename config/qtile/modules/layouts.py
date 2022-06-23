from libqtile import bar, layout, widget
from libqtile.command import lazy
from libqtile.config import Click, DropDown, Group, Key, Match, ScratchPad
from modules.keymaps import keys

from modules.themes import palette

# groups = [
#     Group("1", label="一"),
#     Group("2", label="二"),
#     Group("3", label="三"),
#     Group("4", label="四"),
#     Group("5", label="万"),
#     Group("6", label="六"),
#     Group("7", label="七"),
#     Group("8", label="八"),
#     Group("9", label="九"),
# ]

alt = "mod1"
ctrl = "control"
mod = "mod4"
shift = "shift"
hyper = "mod3"

groups = [Group(f"{i+1}", label="⬤") for i in range(9)]

for i in groups:
    keys.extend(
        [
            # mod1 + letter of group = switch to group
            Key([mod], i.name, lazy.group[i.name].toscreen()),
            # mod1 + shift + letter of group => move focused window to group
            Key([mod, shift], i.name, lazy.window.togroup(i.name)),
        ]
    )

borderline = dict(
    border_focus=palette[8],
    border_normal=palette[1],
    border_width=3,
    margin=12,
)

layouts = [
    layout.MonadTall(**borderline),
    layout.MonadThreeCol(**borderline),
    layout.MonadWide(**borderline),
    layout.Spiral(clockwise=True, main_pane="left", **borderline),
]

# Automatically float application pop-ups
floating_layout = layout.Floating(
    **borderline,
    float_rules=[
        *layout.Floating.default_float_rules,
        Match(wm_class="confirm"),
        Match(wm_class="dialog"),
        Match(wm_class="download"),
        Match(wm_class="error"),
        Match(wm_class="file_progress"),
        Match(wm_class="notification"),
        Match(wm_class="splash"),
        Match(wm_class="toolbar"),
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
        Match(title="Picture-in-Picture"),  # FireFox
        Match(wm_class="ssh-askpass"),  # ssh-askpass
    ],
)

# Scratchpads
# TODO: protonmail -> float on title recognition + scratchpad launch?
next_maximum = {
    "x": 0.05,
    "y": 0.05,
    "width": 0.9,
    "height": 0.9,
    "opacity": 1.0,
}

groups.append(
    ScratchPad(
        "Scratchpad",
        [
            DropDown("Bottom", "kitty -T Bottom -e btm", **next_maximum),
            DropDown("Discord", "discord", **next_maximum),
            DropDown("Emacs", "emacsclient -c", **next_maximum),
            DropDown("Neovide", "neovide", **next_maximum),
            DropDown("Spotify", "spotify", **next_maximum),
            DropDown("Transmission", "transmission-gtk", **next_maximum),
        ],
    )
)

keys.extend(
    [
        Key([mod, alt], "b", lazy.group["Scratchpad"].dropdown_toggle("Bottom")),
        Key([mod, alt], "d", lazy.group["Scratchpad"].dropdown_toggle("Discord")),
        # Key([mod, alt], "e", lazy.group["Scratchpad"].dropdown_toggle("Emacs")),
        Key([mod, alt], "v", lazy.group["Scratchpad"].dropdown_toggle("Neovide")),
        Key([mod, alt], "s", lazy.group["Scratchpad"].dropdown_toggle("Spotify")),
        Key(
            [mod, alt],
            "t",
            lazy.group["Scratchpad"].dropdown_toggle("Transmission"),
        ),
    ]
)
