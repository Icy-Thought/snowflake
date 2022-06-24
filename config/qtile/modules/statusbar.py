# credits: the-argus
# TODO: my own statusbar, use this to setup qtile for now.

from libqtile import bar, qtile, widget
from libqtile.config import Screen
from libqtile.lazy import lazy
from modules.layouts import borderline

from modules.themes import palette

fontinfo = dict(
    font="VictorMono Nerd Font Semibold",
    fontsize=12,
    padding=3,
)

rofi = "rofi -no-lazy-grab -show drun -modi drun"

groupbox = [
    widget.GroupBox,
    {
        "active": palette[0],
        "block_highlight_text_color": palette[0],
        "disable_drag": True,
        "font": fontinfo["font"],
        "fontsize": fontinfo["fontsize"],
        "foreground": palette[2],
        "hide_unused": False,
        "highlight_color": [palette[0], palette[3]],
        "highlight_method": "text",
        "inactive": palette[0],
        "padding": fontinfo["padding"],
        "rounded": False,
        "spacing": 5,
        "this_current_screen_border": palette[4],
        "urgent_alert_method": "line",
        "urgent_border": palette[7],
        "urgent_text": palette[7],
        "use_mouse_wheel": False,
    },
]

windowname = [
    widget.WindowName,
    {
        "background": palette[2],
        "center_aligned": True,
        "font": fontinfo["font"],
        "fontsize": fontinfo["fontsize"],
        "format": "{name}",
        "max_chars": 35,
        "padding": 3,
    },
]

systray = [
    widget.Systray,
    {
        "background": palette[2],
        "foreground": palette[5],
        # "theme_path": "",
    },
]

spacer_small = [
    widget.Spacer,
    {
        "length": 5,
        # these values are used by style func, not qtile
        "inheirit": True,
        "is_spacer": True,
        "use_separator": False,
    },
]

logo = [
    widget.TextBox,
    {
        "font": fontinfo["font"],
        "background": palette[8],
        "fontsize": fontinfo["fontsize"] * 1.6,
        "foreground": palette[1],
        "mouse_callbacks": {"Button1": lazy.spawn(rofi)},
        "padding": -2,
        "text": "  ",
    },
]

layout = [
    widget.CurrentLayout,
    {**fontinfo, "background": palette[3], "foreground": palette[1]},
]

cpu = [
    widget.CPU,
    {
        **fontinfo,
        "background": palette[10],
        "foreground": palette[1],
        "format": " {freq_current}GHz {load_percent}%",
    },
]

net = [
    widget.Net,
    {
        **fontinfo,
        "background": palette[4],
        "format": "\u2193 {down} \u2191 {up}",
        "interface": "wlan0",
        "update_interval": 3,
    },
]

mem = [
    widget.Memory,
    {
        **fontinfo,
        "format": ": {MemUsed:.2f}/{MemTotal:.2f}{mm}",
        "measure_mem": "G",
        "update_interval": 1.0,
    },
]

mpris = [
    widget.Mpris2,
    {
        **fontinfo,
        "foreground": palette[1],
        "background": palette[9],
        "max_chars": 15,
        "paused_text": "\uf8e4 {track}",
        "playing_text": "\uf90b {track}",
    },
]

batt = [
    widget.Battery,
    {
        **fontinfo,
        "background": palette[5],
        "foreground": palette[1],
        "charge_char": "\uf583 ",
        "discharge_char": "",
        "empty_char": "\uf244 ",
        "format": "{char} {percent:2.0%} ",
        "full_char": "\uf240 ",
        "low_background": palette[7],
        "low_foreground": palette[1],
        "low_percentage": 0.30,
        "show_short_text": False,
        "unknown_char": "\uf590 ",
    },
]

datetime = [
    widget.Clock,
    {
        **fontinfo,
        "background": palette[6],
        "foreground": palette[1],
        "format": "%B %d, %H:%M",
    },
]


def widgetlist():
    return [
        spacer_small,
        logo,
        groupbox,
        layout,
        windowname,
        mpris,
        cpu,
        # net,
        mem,
        systray,
        datetime,
        batt,
    ]


def style(widgetlist):
    styled = widgetlist[:]

    for index, wid in enumerate(widgetlist):
        end_sep = {
            "font": fontinfo["font"],
            "fontsize": 34,
            "padding": -1,
            "text": " ",
        }

        if index < len(widgetlist) - 1:
            # end_sep["background"]=widgetlist[index+1][1].get("background", palette[1])
            # end_sep["foreground"]=wid[1].get("background", palette[1])

            end_sep["foreground"] = widgetlist[index + 1][1].get(
                "background", palette[1]
            )
            end_sep["background"] = wid[1].get("background", palette[1])

            if wid[1].get("is_spacer") and wid[1].get("inheirit"):
                bg = widgetlist[index + 1][1].get("background", palette[1])
                wid[1]["background"] = bg
                end_sep["background"] = bg

            # insert separator before current
            if wid[1].get("use_separator", True):
                styled.insert(styled.index(wid) + 1, (widget.TextBox, end_sep))

    return [w[0](**w[1]) for w in styled]


def my_bar():
    return bar.Bar(
        [*style(widgetlist())],
        34,
        foreground=palette[0],
        background=palette[1],
        opacity=1.0,
        margin=[
            borderline["margin"],
            borderline["margin"],
            borderline["border_width"],
            borderline["margin"],
        ],
    )


widget_defaults = dict(
    **fontinfo,
)

extension_defaults = widget_defaults.copy()

screens = [
    Screen(top=my_bar()),
]
