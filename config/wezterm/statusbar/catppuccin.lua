local wez = require("wezterm")

wez.on("update-right-status", function(window, pane)
    local datetime = " 󰃰  " .. wez.strftime("%B %e, %H:%M  ")

    window:set_right_status(wez.format({
        -- { Attribute = { Underline = "Single" } },
        { Attribute = { Italic = true } },
        { Foreground = { Color = "#f5c2e7" } },
        { Text = datetime },
    }))

    window:set_left_status(wez.format({
        { Background = { Color = "#C7C9FD" } },
        { Foreground = { Color = "#1E1D2F" } },
        { Text = "    " },
    }) .. wez.format({
        { Background = { Color = "#1E1D2F" } },
        { Foreground = { Color = "#C7C9FD" } },
        { Text = "" },
    }))
end)

wez.on("format-tab-title", function(tab, tabs, panes, config, hover, max_width)
    local background = "#1e1d2f"
    local foreground = "#7976ab"
    local symbolic = " "

    if tab.is_active then
        background = "#1e1d2f"
        foreground = "#f2cdcd"
        symbolic = " 󰪥"
    elseif hover then
        background = "#3b3052"
        foreground = "#909090"
    end

    local edge_background = background
    local edge_foreground = "#7976ab"
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
