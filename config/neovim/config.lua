-- (Default LunarVim) Config-File -> To-be-replaced.
-- general
lvim.format_on_save = true
lvim.colorscheme = "onedarker"

-- Leader + Keymapping
lvim.leader = "space"
lvim.keys.normal_mode["<C-s>"] = ":w<cr>"

-- Configure builtin plugins
lvim.builtin.dashboard.active = true
lvim.builtin.notify.active = true
lvim.builtin.terminal.active = true

-- Treesitter parsers change this to a table of the languages you want i.e. {"java", "python", javascript}
lvim.builtin.treesitter.ensure_installed = "maintained"
lvim.builtin.treesitter.ignore_install = { "haskell" }

-- Disable virtual text
lvim.lsp.diagnostics.virtual_text = false

-- Select which servers should be configured manually. Requires `:LvimCacheRest` to take effect.
-- See the full default list `:lua print(vim.inspect(lvim.lsp.override))`
vim.list_extend(lvim.lsp.override, { "pyright" })

-- set a formatter, this will override the language server formatting capabilities (if it exists)
local formatters = require "lvim.lsp.null-ls.formatters"
formatters.setup {
  { command = "black" },
  {
    command = "prettier",
    ---@usage specify which filetypes to enable. By default a providers will attach to all the filetypes it supports.
    filetypes = { "typescript", "typescriptreact" },
  },
}

-- set additional linters
local linters = require "lvim.lsp.null-ls.linters"
linters.setup {
  { command = "black" },
  {
    command = "eslint_d",
    ---@usage specify which filetypes to enable. By default a providers will attach to all the filetypes it supports.
    filetypes = { "javascript", "javascriptreact" },
  },
}


-- Additional Plugins
lvim.plugins = {
    {"lunarvim/colorschemes"},
    {"folke/tokyonight.nvim"}, {
        "ray-x/lsp_signature.nvim",
        config = function() require"lsp_signature".on_attach() end,
        event = "BufRead"
    }
}
