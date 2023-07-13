local wez = require("wezterm")

wez.on("update-right-status", function(window, pane)
    local datetime = "   " .. wez.strftime("%B %e, %H:%M  ")

    window:set_right_status(wez.format({
        -- { Attribute = { Underline = "Single" } },
        { Attribute = { Italic = true } },
        { Foreground = { Color = "#DCD7BA" } },
        { Text = datetime },
    }))

    window:set_left_status(wez.format({
        { Background = { Color = "#76946A" } },
        { Foreground = { Color = "#16161D" } },
        { Text = "    " },
    }) .. wez.format({
        { Background = { Color = "#16161D" } },
        { Foreground = { Color = "#DCA561" } },
        { Text = "" },
    }))
end)

wez.on("format-tab-title", function(tab, tabs, panes, config, hover, max_width)
    local background = "#16161D"
    local foreground = "#DCD7BA"
    local symbolic = " "

    if tab.is_active then
        background = "#16161D"
        foreground = "#6A9589"
        symbolic = " 󰪥"
    elseif hover then
        background = "#363646"
        foreground = "#957FB8"
    end

    local edge_background = background
    local edge_foreground = "#7FB4CA"
    local separator = " ⊣ "

    -- ensure that the titles fit in the available space,
    -- and that we have room for the edges.
    local title = wez.truncate_right(tab.active_pane.title, max_width - 5)
        .. "…"

    return {
        -- Separator
        { Background = { Color = edge_background } },
        { Foreground = { Color = edge_foreground } },
        -- Active / Inactive
        { Background = { Color = background } },
        { Foreground = { Color = foreground } },
        { Text = symbolic .. " " .. title },
        -- Separator
        { Background = { Color = edge_background } },
        { Foreground = { Color = edge_foreground } },
        { Text = separator },
    }
end)

return {}
