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

function get_config(name)
	return string.format("require(\"settings/%s\")", name)
end

M.source = debug.getinfo(1, "S").source:sub(2)

M.plugins = {
	{ 
		"rebelot/kanagawa.nvim",
		config = get_config("colorscheme").kanagawa,
	},
	{ 
		"catppuccin/nvim",
		as = "catppuccin",
		config = get_config("colorscheme").catppuccin,
	},
	{
		"gelguy/wilder.nvim",
		event = "CmdlineEnter",
		config = get_config("wilder"),
		requires = { { "romgrk/fzy-lua-native", after = "wilder.nvim" } },
	},
}

return M

-- vim: sw=2 sts=2 ts=2 noexpandtab
