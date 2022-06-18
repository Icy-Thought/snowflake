from libqtile import bar, layout, widget
from libqtile.config import Group, Match, Screen

groups = [
    Group("1", label="一"),
    Group("2", label="二"),
    Group("3", label="三"),
    Group("4", label="四"),
    Group("5", label="万"),
    Group("6", label="六"),
    Group("7", label="七"),
    Group("8", label="八"),
    Group("9", label="九"),
]

for i in groups:
    keys.extend(
        [
            # mod1 + letter of group = switch to group
            Key([mod], i.name, lazy.group[i.name].toscreen()),
            # mod1 + shift + letter of group = switch to & move focused window to group
            Key([mod, shift], i.name, lazy.window.togroup(i.name)),
        ]
    )

border = dict(
    border_focus="#C9CBFF",
    border_normal="#1A1B25",
    border_width=2,
)

layouts = [
    layout.MonadTall(margin=4, **border),
    layout.MonadThreeCol(margin=4, **border),
    layout.MonadWide(margin=4, **border),
    layout.Spiral(clockwise=True, main_pane="left", margin=4, **border),
]

# Automatically float application pop-ups
floating_layout = layout.Floating(
    **border,
    float_rules=[
        *layout.Floating.default_float_rules,
        Match(wmclass="confirm"),
        Match(wmclass="dialog"),
        Match(wmclass="download"),
        Match(wmclass="error"),
        Match(wmclass="file_progress"),
        Match(wmclass="notification"),
        Match(wmclass="splash"),
        Match(wmclass="toolbar"),
        Match(wmclass="confirmreset"),  # gitk
        Match(wmclass="makebranch"),  # gitk
        Match(wmclass="maketag"),  # gitk
        Match(wname="branchdialog"),  # gitk
        Match(wname="pinentry"),  # GPG key password entry
        Match(wname="Picture-in-Picture"),  # FireFox
        Match(wmclass="ssh-askpass"),  # ssh-askpass
    ]
)

# Scratchpads
# TODO: protonmail -> float on title recognition + scratchpad launch?
default_float = Dict(x=0.05, y=0.05, width=0.9, height=0.9, opacity=1.0)

groups.append(
    ScratchPad(
        "Scratchpad",
        [
            DropDown("Bottom", "kitty -T Bottom -e btm", **default_float),
            DropDown("Discord", "discord", **default_float),
            # DropDown("Emacs", "emacsclient -c", **default_float),
            DropDown("Neovide", "neovide", **default_float),
            DropDown("Spotify", "spotify", **default_float),
            DropDown("Transmission", "transmission-gtk", **default_float),
        ],
    )
)

mod = "mod4"
alt = "mod1"

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
