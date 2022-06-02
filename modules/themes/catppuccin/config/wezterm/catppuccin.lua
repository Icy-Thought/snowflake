{ colors, font, ... }: ''

return {
    foreground = "${colors.types.fg}",
    background = "${colors.types.bg}",

    cursor_fg = "${colors.types.bg}",
    cursor_bg = "${colors.yellow}",
    cursor_border = "${colors.yellow}",

    selection_fg = "${colors.types.bg}",
    selection_bg = "${colors.types.highlight}",

    scrollbar_thumb = "${colors.magenta}",
    split = "${colors.green}",

    tab_bar = {
        active_tab = {
            bg_color = "${colors.types.bg}",
            fg_color = "${colors.magenta}",
            intensity = "Normal",
            italic = true,
            underline = "Single",
        },
        inactive_tab = {
            bg_color = "${colors.types.bg}",
            fg_color = "${colors.types.fg}",
            italic = true,
        },
        inactive_tab_edge = "${colors.black}",
        inactive_tab_hover = {
            bg_color = "${colors.types.bg}",
            fg_color = "${colors.yellow}",
            italic = true,
            underline = "Single",
        },
        new_tab = {
            bg_color = "${colors.types.bg}",
            fg_color = "${colors.green}",
            italic = true,
        },
        new_tab_hover = {
            bg_color = "${colors.types.bg}",
            fg_color = "${colors.yellow}",
            italic = true,
        },
    },

    ansi = {
        "${colors.black}",
        "${colors.red}",
        "${colors.green}",
        "${colors.yellow}",
        "${colors.blue}",
        "${colors.magenta}",
        "${colors.cyan}",
        "${colors.white}",
    },

    brights = {
        "${colors.brightBlack}",
        "${colors.brightRed}",
        "${colors.brightGreen}",
        "${colors.brightYellow}",
        "${colors.brightBlue}",
        "${colors.brightMagenta}",
        "${colors.brightCyan}",
        "${colors.brightWhite}",
    },
}
''
