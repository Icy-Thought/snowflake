# credits: biscuitrescue
# TODO: my own statusbar, use this to setup qtile for now.

import os

from libqtile import bar, hook, layout, widget
from libqtile.config import Screen
from qtile_extras import widget
from qtile_extras.bar import Bar
from qtile_extras.widget.decorations import RectDecoration

active = "catppuccin"

colours = {
    "catppuccin": [
        ["#D8DEE9"],  # Colour 0
        ["#1e1e2e"],  # Colour 1
        ["#f28fad"],  # Colour 2
        ["#abe9b3"],  # Colour 3
        ["#fae3b0"],  # Colour 4
        ["#d6acff"],  # Colour 5
        ["#f5c2e7"],  # Colour 6
        ["#89DCEB"],  # Colour 7
        ["#C9CBFF"],  # Colour 8
        ["#b5e8e0"],  # Colour 9
        ["#F2779C"],  # Colour 10
    ],
}

decor = {
    "decorations": [
        RectDecoration(
            use_widget_background=True,
            radius=11,
            filled=True,
            padding_y=8,
        )
    ],
    "padding": 10,
}
decor1 = {
    "decorations": [
        RectDecoration(
            use_widget_background=True,
            radius=[11, 0, 0, 11],
            filled=True,
            padding_y=8,
        )
    ],
    "padding": 10,
}
decor2 = {
    "decorations": [
        RectDecoration(
            use_widget_background=True,
            radius=[0, 11, 11, 0],
            filled=True,
            padding_y=8,
        )
    ],
    "padding": 10,
}


font_size = 12
font_family = "VictorMono Nerd Font SemiBold"

default = [
    widget.GroupBox(
        font=font_family,
        fontsize=25,
        background=colours[active][1],
        margin_y=4,
        margin_x=5,
        padding_y=3,
        padding_x=2,
        borderwidth=8,
        inactive=colours[active][8],
        active=colours[active][4],
        rounded=True,
        # invert_mouse_wheel=True,
        urgent_alert_method="text",
        urgent_text=colours[active][10],
        highlight_color=colours[active][4],
        highlight_method="text",
        this_current_screen_border=colours[active][2],
        block_highlight_text_color=colours[active][1],
    ),
    widget.Sep(
        padding=2,
        linewidth=0,
    ),
    widget.CurrentLayoutIcon(
        custom_icon_paths=[
            os.path.expanduser("~/git/Icy-Thought/Snowflake/config/qtile/icons")
        ],
        scale=0.45,
        background=colours[active][1],
    ),
    widget.Spacer(),
    widget.Systray(
        icon_size=20,
        padding=4,
    ),
    widget.TextBox(
        foreground=colours[active][9],
        text="|",
        font=font_family,
    ),
    widget.CPU(
        background=colours[active][9],
        foreground=colours[active][1],
        format="  {load_percent}%",
        font=font_family,
        fontsize=font_size,
        **decor,
    ),
    widget.TextBox(
        foreground=colours[active][4],
        text="|",
        font=font_family,
    ),
    widget.Memory(
        font=font_family,
        fontsize=font_size,
        background=colours[active][4],
        foreground=colours[active][1],
        measure_mem="G",
        measure_swap="G",
        format="  {MemUsed: .2f} GB",
        **decor,
    ),
    widget.TextBox(
        foreground=colours[active][6],
        text="|",
        font=font_family,
    ),
    widget.Memory(
        measure_mem="G",
        font=font_family,
        fontsize=font_size,
        foreground=colours[active][1],
        background=colours[active][6],
        measure_swap="G",
        format=" {SwapUsed: .2f} GB",
        **decor,
    ),
    widget.TextBox(
        foreground=colours[active][3],
        text="|",
        font=font_family,
    ),
    widget.Volume(
        mouse_callbacks={"Button3": lambda: qtile.cmd_spawn("pavucontrol")},
        background=colours[active][3],
        foreground=colours[active][1],
        update_interval=0.001,
        font=font_family,
        fontsize=font_size,
        **decor,
    ),
    widget.TextBox(
        foreground=colours[active][8],
        text="|",
        font=font_family,
    ),
    widget.Clock(
        foreground=colours[active][1],
        background=colours[active][8],
        format="  %d %b, %a",
        font=font_family,
        fontsize=font_size,
        **decor,
    ),
    widget.TextBox(
        foreground=colours[active][5],
        text="|",
        font=font_family,
    ),
    widget.Clock(
        foreground=colours[active][1],
        background=colours[active][5],
        font=font_family,
        fontsize=font_size,
        format="  %I:%M %p",
        **decor,
    ),
    widget.TextBox(
        foreground=colours[active][7],
        text="|",
        font=font_family,
    ),
]
if len(os.listdir("/sys/class/power_supply")) == 0:
    default.extend(
        [
            widget.CapsNumLockIndicator(
                fontsize=font_size,
                font=font_family,
                foreground=colours[active][1],
                background=colours[active][7],
                **decor,
            ),
            widget.TextBox(
                foreground=colours[active][7],
                text="|",
                font=font_family,
            ),
        ]
    )
else:
    default.extend(
        [
            widget.UPowerWidget(
                font=font_family,
                battery_width=23,
                battery_height=10,
                fontsize=font_size,
                percentage_low=0.5,
                percentage_critical=0.3,
                fill_critical="#ff0000",
                fill_low=colours[active][4],
                fill_normal=colours[active][1],
                background=colours[active][7],
                border_colour=colours[active][1],
                border_critical_colour=colours[active][1],
                border_charge_colour=colours[active][1],
                text_charging="",
                text_discharging="",
                text_displaytime="",
                margin=6,
                **decor1,
            ),
            widget.Battery(
                fontsize=font_size,
                font=font_family,
                low_percentage=0.25,
                low_background=colours[active][7],
                low_foreground=colours[active][1],
                foreground=colours[active][1],
                background=colours[active][7],
                charge_char="↑",
                discharge_char="",
                update_interval=1,
                format="{percent:2.0%}{char}",
                **decor2,
            ),
            widget.TextBox(
                foreground=colours[active][7],
                text="|",
                font=font_family,
            ),
        ]
    )

screens = [
    Screen(
        top=bar.Bar(
            default,
            44,
            background=colours[active][1],
            foreground=colours[active][1],
            # opacity=0.9,
            margin=[8, 10, 2, 10],
        ),
    ),
]
