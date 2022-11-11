-- Copyright (c) 2022 Jaawerth. All Rights Reserved.

local wez = require("wezterm")

wez.on("gui-startup", function(cmd)
    local win_opts = { args = cmd and cmd.args or {} }

    -- parse the -t arg and set a title var on GLOBAL so it survives config reload
    if win_opts.args[1] == "-t" then
        table.remove(win_opts.args, 1)
        wez.GLOBAL.window_title = table.remove(win_opts.args, 1)
    end

    -- set win title to supplied window title (should survive config reloads this way)
    wez.on(
        "format-window-title",
        function(tab) -- extra args: pane, tabs, panes, config
            return wez.GLOBAL.window_title or tab.active_pane.title
        end
    )

    -- now, spawn the initial window:
    wez.mux.spawn_window(win_opts)
end)

return {}
