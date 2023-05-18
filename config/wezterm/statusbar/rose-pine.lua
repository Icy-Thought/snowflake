local wez = require("wezterm")

wez.on("update-right-status", function(window, pane)
    local datetime = "   " .. wez.strftime("%B %e, %H:%M  ")

    window:set_right_status(wez.format({
        -- { Attribute = { Underline = "Single" } },
        { Attribute = { Italic = true } },
        { Foreground = { Color = "#f9d6d2" } },
        { Text = datetime },
    }))

    window:set_left_status(wez.format({
        { Background = { Color = "#9ccfd8" } },
        { Foreground = { Color = "#191724" } },
        { Text = "    " },
    }) .. wez.format({
        { Background = { Color = "#191724" } },
        { Foreground = { Color = "#9ccfd8" } },
        { Text = "" },
    }))
end)

wez.on("format-tab-title", function(tab, tabs, panes, config, hover, max_width)
    local background = "#191724"
    local foreground = "#e0def4"
    local symbolic = " "

    if tab.is_active then
        background = "#1a1b26"
        foreground = "#bb7b79"
        symbolic = " 󰪥"
    elseif hover then
        background = "#3b3052"
        foreground = "#9ccfd8"
    end

    local edge_background = background
    local edge_foreground = "#ebbcba"
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
