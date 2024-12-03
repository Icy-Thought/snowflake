# Qtile
from lib.keymaps import keys
from lib.themes import palette
from libqtile import bar, layout, widget
from libqtile.command import lazy
from libqtile.config import DropDown, EzClick, EzKey, Group, Match, ScratchPad

# Requires CJK-font (using: Sarasa Gothic)
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
            EzKey("M-%s" % i.name, lazy.group[i.name].toscreen()),
            # mod1 + shift + letter of group => move focused window to group
            EzKey("M-S-%s" % i.name, lazy.window.togroup(i.name)),
        ]
    )

borderline = dict(
    border_focus=palette[8],
    border_normal=palette[1],
    border_width=2,
    margin=10,
)

layouts = [
    layout.MonadTall(**borderline),
    layout.MonadThreeCol(**borderline),
    layout.MonadWide(**borderline, ratio=0.65),
    layout.Max(**borderline),
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
        Match(wm_class="pinentry"),  # GPG key password entry
        Match(title="Picture-in-Picture"),  # Zen Browser
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(wm_class="leagueclientux.exe"),
    ],
)

# Scratchpads
# TODO: protonmail -> float on title recognition + scratchpad launch?
next_maximum = {
    "x": 0.02,
    "y": 0.02,
    "width": 0.95,
    "height": 0.95,
    "opacity": 1.00,
    "on_focus_lost_hide": False,
}

groups.append(
    ScratchPad(
        "SPD",
        [
            DropDown(
                "SysMon",
                "alacritty --class 'system-monitor' -e btop",
                **next_maximum,
            ),
            DropDown(
                "Discord",
                "discord",
                match=Match(wm_class="discord"),
                **next_maximum,
            ),
            DropDown(
                "Matrix",
                "emacsclient -c -a '' -F 'ement-scratch' --eval '(ement-room-list)'",
                match=Match(title="ement-scratch"),
                **next_maximum,
            ),
            DropDown(
                "Emacs",
                "emacsclient -c -a '' -F 'emacs-scratch' --eval '(dashboard-refresh-buffer)'",
                match=Match(title="emacs-scratch"),
                **next_maximum,
            ),
            DropDown("Galaxy Buds Manager", "GalaxyBudsClient", **next_maximum),
            DropDown(
                "Neovide",
                "neovide --multigrid --frame none",
                match=Match(wm_class="neovide"),
                **next_maximum,
            ),
            DropDown(
                "Music",
                "youtube-music",
                **next_maximum,
                match=Match(wm_class="YouTube Music"),
            ),
            DropDown(
                "Telegram",
                "emacsclient -c -a '' -F 'telega-scratch' --eval '(telega)'",
                match=Match(title="telega-scratch"),
                **next_maximum,
            ),
            DropDown("Transmission", "transmission-gtk", **next_maximum),
            DropDown("SoundCTL", "eassyeffects", **next_maximum),
        ],
    )
)

keys.extend(
    # :WARN| Used keys (M-A): f = Zen Browser, w = Zen Browser Private
    [
        EzKey("M-A-b", lazy.group["SPD"].dropdown_toggle("SysMon")),
        EzKey("M-A-e", lazy.group["SPD"].dropdown_toggle("Emacs")),
        EzKey("M-A-k", lazy.group["SPD"].dropdown_toggle("Matrix")),
        EzKey("M-A-d", lazy.group["SPD"].dropdown_toggle("Discord")),
        EzKey("M-A-l", lazy.group["SPD"].dropdown_toggle("Telegram")),
        EzKey("M-A-t", lazy.group["SPD"].dropdown_toggle("Transmission")),
        EzKey("M-A-s", lazy.group["SPD"].dropdown_toggle("Music")),
        EzKey("M-A-v", lazy.group["SPD"].dropdown_toggle("SoundCTL")),
    ]
)
