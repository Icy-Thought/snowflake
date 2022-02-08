-- Colorscheme & Settings:
vim.cmd("colorscheme catppuccin")

-- Core functionality:
require "core.plugins"
require "core.options"

-- Keymaps & Autocommands
require "keymaps"
require "modules.editor.autocommands"
