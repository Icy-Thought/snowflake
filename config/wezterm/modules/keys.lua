local wez = require("wezterm")

local M = {}

M.keys = {
    { -- Focus -> left window
        key = "j",
        mods = "ALT",
        action = wez.action({ ActivatePaneDirection = "Left" }),
    },
    { -- Focus -> right window
        key = "k",
        mods = "ALT",
        action = wez.action({ ActivatePaneDirection = "Right" }),
    },
    { -- Cycle prompts -> left
        key = "h",
        mods = "ALT",
        action = wez.action({ ActivateTabRelative = -1 }),
    },
    { -- Cycle prompts -> right
        key = "l",
        mods = "ALT",
        action = wez.action({ ActivateTabRelative = 1 }),
    },
    { -- Split pane -> vertically
        key = "|",
        mods = "CTRL|SHIFT",
        action = wez.action({
            SplitVertical = { domain = "CurrentPaneDomain" },
        }),
    },
    {
        key = "-",
        mods = "CTRL|SHIFT",
        action = wez.action({
            SplitHorizontal = { domain = "CurrentPaneDomain" },
        }),
    },
}

return M
