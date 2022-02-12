local toolset = {}

function get_config(name)
	return string.format("require(\"modules.toolset.config.%s\")", name)
end

toolset["folke/which-key.nvim"] = {
	opt = false,
    event = "BufWinEnter",
	keys = "<space>",
	config = get_config("which-key"),
}
toolset["nvim-telescope/telescope.nvim"] = {
	opt = true,
	module = "telescope",
	cmd = "Telescope",
	config = get_config("telescope"),
	requires = {
		{ "nvim-lua/plenary.nvim", opt = false },
		{ "nvim-lua/popup.nvim", opt = true },
	},
}
toolset["nvim-telescope/telescope-fzf-native.nvim"] = {
	opt = true,
	run = "make",
	after = "telescope.nvim",
}
toolset["nvim-telescope/telescope-project.nvim"] = {
	opt = true,
	after = "telescope.nvim",
}
-- Enable after finding non-nix way to manage sqlite-lua:
-- tools["nvim-telescope/telescope-frecency.nvim"] = {
-- 	opt = true,
-- 	after = "telescope-project.nvim",
-- 	requires = { { "tami5/sqlite.lua", opt = true } },
-- }
toolset["vimlab/split-term.vim"] = {
	opt = true,
	cmd = { "Term", "VTerm" },
}
toolset["akinsho/nvim-toggleterm.lua"] = {
	opt = true,
	event = "BufRead",
	config = get_config("toggle-term"),
}
toolset["numtostr/FTerm.nvim"] = {
	opt = true,
	event = "BufRead",
}


return toolset
