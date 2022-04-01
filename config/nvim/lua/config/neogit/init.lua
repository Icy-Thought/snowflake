local neogit = prequire("neogit")

neogit.setup({
    auto_refresh = true,
    disable_builtin_notifications = false,
    disable_commit_confirmation = false,
    disable_context_highlighting = false,
    disable_hint = false,
    disable_signs = false,
    use_magit_keybindings = false,
    commit_popup = {
        kind = "split",
    },
    kind = "tab",
    signs = {
        -- { CLOSED, OPENED }
        section = { ">", "v" },
        item = { ">", "v" },
        hunk = { "", "" },
    },
    integrations = {
        diffview = false,
    },
    sections = {
        untracked = {
            folded = false,
        },
        unstaged = {
            folded = false,
        },
        staged = {
            folded = false,
        },
        stashes = {
            folded = true,
        },
        unpulled = {
            folded = true,
        },
        unmerged = {
            folded = false,
        },
        recent = {
            folded = true,
        },
    },
    mappings = {
        status = {
            ["B"] = "BranchPopup",
            ["s"] = "",
        },
    },
})
