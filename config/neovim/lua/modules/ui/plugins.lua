local ui = {}

function get_config(name)
	return string.format("require(\"modules.ui.config.%s\")", name)
end

ui["kyazdani42/nvim-web-devicons"] = { opt = false }
ui["rebelot/kanagawa.nvim"] = { 
	opt = false, 
	config = get_config(colorscheme).kanagawa,
}
ui["catppuccin/nvim"] = {
	opt = false,
	as = "catppuccin",
	config = get_config(colorscheme).catppuccin,
}
ui["RishabhRD/popfix"] = { opt = false }
ui["nvim-lua/popup.nvim"] = { opt = true }
ui["glepnir/dashboard-nvim"] = { 
	opt = true,
	event = "BufWinEnter",
	config = get_config("remove-later"),
}
-- ui["startup-nvim/startup.nvim"] = {
-- 	opt = true,
-- 	event = "BufWinEnter",
-- 	config = get_config("dashboard"),
-- 	requires = {"nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim"},
-- }
ui["kyazdani42/nvim-tree.lua"] = {
	opt = true,
	event = "BufRead",
	config = get_config("nvim-tree"),
}
ui["akinsho/bufferline.nvim"] = {
	opt = true,
	event = "BufRead",
	config = get_config("bufferline"),
}
ui["hoob3rt/lualine.nvim"] = {
	opt = true,
	event = "BufRead",
	config = get_config("lualine"),
}
ui["lewis6991/gitsigns.nvim"] = {
	opt = true,
	event = { "BufRead", "BufNewFile" },
	config = get_config("gitsigns"),
	requires = { "nvim-lua/plenary.nvim", opt = true },
}
ui["j-hui/fidget.nvim"] = {
	opt = true,
	event = "BufEnter",
	config = get_config("fidget"),
}
ui["lukas-reineke/indent-blankline.nvim"] = {
	opt = true,
	event = "BufRead",
	config = get_config("indentline"),
}
ui["gelguy/wilder.nvim"] = {
	event = "CmdlineEnter",
	config = get_config("wilder"),
	requires = { { "romgrk/fzy-lua-native", after = "wilder.nvim" } },
}

return ui
