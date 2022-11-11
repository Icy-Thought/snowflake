local wez = require("wezterm")

local M = {}

M.mouse_bindings = {
    {
        event = { Down = { streak = 1, button = "Left" } },
        mods = "NONE",
        action = wez.action({ SelectTextAtMouseCursor = "Cell" }),
    },
    {
        event = { Down = { streak = 2, button = "Left" } },
        mods = "NONE",
        action = wez.action({ SelectTextAtMouseCursor = "Word" }),
    },
    {
        event = { Down = { streak = 3, button = "Left" } },
        mods = "NONE",
        action = wez.action({ SelectTextAtMouseCursor = "Line" }),
    },
    {
        event = { Drag = { streak = 1, button = "Left" } },
        mods = "NONE",
        action = wez.action({ ExtendSelectionToMouseCursor = "Cell" }),
    },
    {
        event = { Drag = { streak = 2, button = "Left" } },
        mods = "NONE",
        action = wez.action({ ExtendSelectionToMouseCursor = "Word" }),
    },
    {
        event = { Drag = { streak = 3, button = "Left" } },
        mods = "NONE",
        action = wez.action({ ExtendSelectionToMouseCursor = "Line" }),
    },
    {
        event = { Up = { streak = 1, button = "Left" } },
        mods = "NONE",
        action = wez.action({
            CompleteSelection = "ClipboardAndPrimarySelection",
        }),
    },
    {
        event = { Up = { streak = 2, button = "Left" } },
        mods = "NONE",
        action = wez.action({
            CompleteSelection = "ClipboardAndPrimarySelection",
        }),
    },
    {
        event = { Up = { streak = 3, button = "Left" } },
        mods = "NONE",
        action = wez.action({ CompleteSelection = "PrimarySelection" }),
    },
    {
        event = { Up = { streak = 1, button = "Left" } },
        mods = "SUPER",
        action = wez.action({
            CompleteSelectionOrOpenLinkAtMouseCursor = "ClipboardAndPrimarySelection",
        }),
    },
}

return M
