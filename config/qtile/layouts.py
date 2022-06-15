from libqtile import bar, layout, widget
from libqtile.config import Group, Screen

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

activeBorder = "#C9CBFF"
inactiveBorder = "#1A1B25"

layout_palette = {
    "border_focus": activeBorder,
    "border_normal": inactiveBorder,
    "border_width": 2,
    "margin": 4,
}

layouts = [
    layout.MonadTall(**layout_palette),
    layout.MonadThreeCol(**layout_palette),
    layout.MonadWide(**layout_palette),
    layout.Spiral(
        **layout_palette,
        clockwise=True,
        main_pane="left",
    ),
]

widget_defaults = dict(
    font="VictorMono Nerd Font SemiBold",
    fontsize=12,
    padding=5,
)
extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        bottom=bar.Bar(
            [
                widget.GroupBox(),
                widget.Prompt(),
                widget.WindowName(),
                widget.TextBox("default config", name="default"),
                widget.Systray(),
                widget.Clock(format="%Y-%m-%d %a %I:%M %p"),
            ],
            24,
        ),
    ),
]

floating_layout = layout.Floating(
    float_rules=[
        {"wmclass": "confirm"},
        {"wmclass": "dialog"},
        {"wmclass": "download"},
        {"wmclass": "error"},
        {"wmclass": "file_progress"},
        {"wmclass": "notification"},
        {"wmclass": "splash"},
        {"wmclass": "toolbar"},
        {"wmclass": "confirmreset"},  # gitk
        {"wmclass": "makebranch"},  # gitk
        {"wmclass": "maketag"},  # gitk
        {"wname": "branchdialog"},  # gitk
        {"wname": "pinentry"},  # GPG key password entry
        {"wmclass": "ssh-askpass"},  # ssh-askpass
    ]
)
