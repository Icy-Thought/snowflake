# Author: moonlightsh
# https://github.com/kovidgoyal/kitty/discussions/4447#discussioncomment-7771377

import datetime
from kitty.fast_data_types import Screen, get_options
from kitty.tab_bar import (
    DrawData,
    ExtraData,
    TabBarData,
    as_rgb,
    draw_tab_with_powerline,
)


CLOCK_FG = as_rgb(int("1e1d2f", 16))
CLOCK_BG = as_rgb(int("c9cbff", 16))

DATE_FG = CLOCK_FG
DATE_BG = as_rgb(int("f2cdcd", 16))

SYMBOL = "     "
SYMBOL_FG = CLOCK_FG
SYMBOL_BG = as_rgb(int("96cdfb", 16))


def draw_icon(screen: Screen, index: int, symbol: str = "") -> int:
    if index != 1:
        return screen.cursor.x

    cells = [
        (SYMBOL_FG, SYMBOL_BG, symbol),
        (SYMBOL_BG, SYMBOL_FG, ""),
        (SYMBOL_FG, screen.cursor.bg, " "),
    ]

    for fg, bg, cell in cells:
        restore_fg, restore_bg = screen.cursor.fg, screen.cursor.bg
        screen.cursor.fg, screen.cursor.bg = fg, bg
        screen.draw(cell)
        screen.cursor.fg, screen.cursor.bg = SYMBOL_BG, restore_bg

    return screen.cursor.x


def draw_right_status(screen: Screen, is_last: bool) -> int:
    if not is_last:
        return screen.cursor.x

    cells = [
        (CLOCK_BG, screen.cursor.bg, ""),
        (CLOCK_FG, CLOCK_BG, datetime.datetime.now().strftime("  %H:%M ")),
        (DATE_BG, CLOCK_BG, ""),
        (DATE_FG, DATE_BG, datetime.datetime.now().strftime("  %Y/%m/%d ")),
    ]

    right_status_length = 0
    for _, _, cell in cells:
        right_status_length += len(cell)

    draw_spaces = screen.columns - screen.cursor.x - right_status_length
    if draw_spaces > 0:
        screen.draw(" " * draw_spaces)

    for fg, bg, cell in cells:
        screen.cursor.fg = fg
        screen.cursor.bg = bg
        screen.draw(cell)

    screen.cursor.fg = 0
    screen.cursor.bg = 0
    screen.cursor.x = max(screen.cursor.x, screen.columns - right_status_length)
    return screen.cursor.x


def draw_tab(
    draw_data: DrawData,
    screen: Screen,
    tab: TabBarData,
    before: int,
    max_title_length: int,
    index: int,
    is_last: bool,
    extra_data: ExtraData,
) -> int:
    draw_icon(screen, index, symbol=SYMBOL)
    end = draw_tab_with_powerline(
        draw_data, screen, tab, before, max_title_length, index, is_last, extra_data
    )
    draw_right_status(screen, is_last)
    return end
