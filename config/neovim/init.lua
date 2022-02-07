-- Colorscheme & Settings:
vim.cmd("colorscheme catppuccin")

-- Core functionality:
require "core.alpha"
require "core.options"
require "core.plugins"

-- Keymaps:
require "keymaps"

-- LSP-Completion:
require "modules.completion"
require "modules.completion.autopairs"
require "modules.completion.luasnip"

-- Editor-related settings:
require "modules.editor.autocommands"
require "modules.editor.impatient"
require "modules.editor.whichkey"
require "modules.editor.comment"
require "modules.editor.indentline"
require "modules.editor.project"
require "modules.editor.telescope"
require "modules.editor.treesitter"
require "modules.editor.nvim-tree"
require "modules.editor.toggleterm"

-- UI-related Settings:
require "modules.ui.lualine"
require "modules.ui.bufferline"
require "modules.ui.gitsigns"
