-- doom_userplugins - Doom nvim custom plugins
--
-- All the available fields can be found here:
-- https://github.com/wbthomason/packer.nvim#specifying-plugins
--
-- By example, for including a plugin with a dependency on telescope:
-- M.plugins {
--   {
--     'user/repository',
--     requires = { 'nvim-lua/telescope.nvim' },
--   },
-- }

local M = {}

M.source = debug.getinfo(1, "S").source:sub(2)

M.plugins = {
	{ "rebelot/kanagawa.nvim" },
	{ "catppuccin/nvim", as = "catppuccin" }
}

return M

-- vim: sw=2 sts=2 ts=2 noexpandtab
