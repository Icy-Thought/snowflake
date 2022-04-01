local indent_blankline = prequire("indent_blankline")

-- Lazy load indent-blankline
vim.cmd("autocmd CursorMoved * IndentBlanklineRefresh")

vim.opt.list = true
vim.opt.listchars:append("space:⋅")
vim.opt.listchars:append("eol:↴")

indent_blankline.setup {
    char = "│",
    space_char_blankline = " ",
    show_first_indent_level = true,
    show_trailing_blankline_indent = false,
    filetype_exclude = {
        "startify",
        "dashboard",
        "git",
        "gitcommit",
        "help",
        "json",
        "log",
        "markdown",
        "NvimTree",
        "Octo",
        "TelescopePrompt",
        "Trouble",
        "txt",
        "undotree",
        "",
    },
    buftype_exclude = { "terminal", "nofile" },
    show_current_context = true,
    show_current_context_start = true,
    context_patterns = {
        "class",
        "function",
        "method",
        "block",
        "list_literal",
        "selector",
        "^if",
        "^table",
        "if_statement",
        "while",
        "for",
        "type",
        "var",
        "import",
    },
}
