# credits: the-argus
# TODO: my own statusbar, use this to setup qtile for now.

from libqtile import bar, qtile, widget
from libqtile.config import Screen
from libqtile.lazy import lazy
from modules.layouts import borderline

from modules.themes import palette

font = dict(
    family="VictorMono Nerd Font Semibold",
    size=12,
    padding=3,
)

rofi = "rofi -no-lazy-grab -show drun -modi drun"

groupbox = [
    widget.GroupBox,
    {
        "font": font["family"],
        "padding": font["padding"],
        "fontsize": font["size"],
        "foreground": palette[2],
        "highlight_method": "text",
        "block_highlight_text_color": palette[0],
        "active": palette[0],
        "inactive": palette[0],
        "rounded": False,
        "highlight_color": [palette[0], palette[3]],
        "urgent_alert_method": "line",
        "urgent_text": palette[7],
        "urgent_border": palette[7],
        "disable_drag": True,
        "use_mouse_wheel": False,
        "hide_unused": False,
        "spacing": 5,
        "this_current_screen_border": palette[10],
    },
]

windowname = [
    widget.WindowName,
    {
        "font": font["family"],
        "fontsize": 16,
        "padding": 3,
        "format": "{name}",
        "background": palette[0],
        "center_aligned": True,
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
        "is_spacer": True,
        "inheirit": True,
        "use_separator": False,
    },
]

logo = [
    widget.TextBox,
    {
        # text="  ",
        "font": font["family"],
        "padding": -2,
        "fontsize": font["size"] * 1.6,
        "text": " ",
        # "text": " Σ",
        "background": palette[8],
        "foreground": palette[1],
        "mouse_callbacks": {"Button1": lazy.spawn(rofi)},
    },
]

layout = [widget.CurrentLayout, {**font, "background": palette[1]}]

cpu = [
    widget.CPU,
    {
        **font,
        "format": " {freq_current}GHz {load_percent}%",
        "background": palette[3],
        "foreground": palette[1],
    },
]

net = [
    widget.Net,
    {
        **font,
        "format": "\u2193 {down} \u2191 {up}",
        "interface": "wlan0",
        "update_interval": 3,
        "background": palette[4],
    },
]

mem = [
    widget.Memory,
    {
        **font,
        "format": ": {MemUsed:.2f}/{MemTotal:.2f}{mm}",
        "update_interval": 1.0,
        "measure_mem": "G",
    },
]

batt = [
    widget.Battery,
    {
        **font,
        "background": palette[8],
        "foreground": palette[1],
        "low_foreground": palette[7],
        "low_background": None,
        "low_percentage": 0.30,
        "charge_char": "",
        "discharge_char": "",
        "full_char": "",
        "empty_char": "X",
        "unknown_char": "?",
        "format": "  {char} {percent:2.0%}",
        "show_short_text": False,
    },
]

datetime = [
    widget.Clock,
    {**font, "format": "%B %d, %H:%M", "background": palette[5]},
]


def widgetlist():
    return [
        spacer_small,
        logo,
        groupbox,
        layout,
        windowname,
        cpu,
        net,
        mem,
        batt,
        datetime,
        systray,
    ]


def style(widgetlist):
    # adds separator widgets in between the initial widget list
    styled = widgetlist[:]

    for index, wid in enumerate(widgetlist):
        end_sep = {
            "font": "VictorMono Nerd Font",
            "text": " ",
            "fontsize": 34,
            "padding": -1,
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
        margin=[borderline[4], borderline[4], borderline[3], borderline[4]],
    )


widget_defaults = dict(
    **font,
)

extension_defaults = widget_defaults.copy()

screens = [
    Screen(top=my_bar()),
]
