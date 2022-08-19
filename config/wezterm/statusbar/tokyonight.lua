local wezterm = require("wezterm")

wezterm.on("update-right-status", function(window, pane)
    local datetime = "   " .. wezterm.strftime("%B %e, %H:%M  ")

    window:set_right_status(wezterm.format({
        -- { Attribute = { Underline = "Single" } },
        { Attribute = { Italic = true } },
        { Foreground = { Color = "#ff9e64" } },
        { Text = datetime },
    }))

    window:set_left_status(wezterm.format({
        { Background = { Color = "#1abc9c" } },
        { Foreground = { Color = "#1a1b26" } },
        { Text = "     " },
    }))
end)

wezterm.on(
    "format-tab-title",
    function(tab, tabs, panes, config, hover, max_width)
        local background = "#1a1b26"
        local foreground = "#7aa2f7"
        local symbolic = " ○ "

        if tab.is_active then
            background = "#1a1b26"
            foreground = "#bb9af7"
            symbolic = " 綠 "
        elseif hover then
            background = "#3b3052"
            foreground = "#1abc9c"
        end

        local edge_background = background
        local edge_foreground = "#7976ab"
        local separator = " | "

        -- ensure that the titles fit in the available space,
        -- and that we have room for the edges.
        local title =
            wezterm.truncate_right(tab.active_pane.title, max_width - 5) .. "…"

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
    end
)

return {}
