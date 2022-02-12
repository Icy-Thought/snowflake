local editor = {}

function get_config(name)
	return string.format("require(\"modules.editor.config.%s\")", name)
end

editor["nvim-lua/plenary.nvim"] = { opt = false }
editor["lewis6991/impatient.nvim"] = {
	opt = true,
	config = get_config("impatient"),
}
editor["moll/vim-bbye"] = { opt = true }
editor["rmagatti/auto-session"] = {
	opt = true,
	cmd = { "SaveSession", "RestoreSession", "DeleteSession" },
	config = get_config("auto-session"),
}
editor["norcalli/nvim-colorizer.lua"] = {
	opt = true,
	event = "BufRead",
	config = get_config("colorizer"),
}
editor["rcarriga/nvim-dap-ui"] = {
	opt = false,
	config = get_config("nvim-dap").ui,
	requires = {
		{ "mfussenegger/nvim-dap", config = get_config("nvim-dap").default },
		{ 
			"Pocco81/DAPInstall.nvim",
			opt = true,
			cmd = { "DIInstall", "DIUninstall", "DIList" },
			config = get_config("nvim-dap").install,
		},
	},
}
editor["numToStr/Comment.nvim"] = {
	opt = true,
    event = "BufRead",
	config = get_config("comment"),
}
editor["phaazon/hop.nvim"] = {
	opt = true,
	branch = "v1",
	cmd = {
		"HopLine",
		"HopLineStart",
		"HopWord",
		"HopPattern",
		"HopChar1",
		"HopChar2",
	},
	config = function()
		require("hop").setup({ keys = "etovxqpdygfblzhckisuran" })
	end,
}
editor["edluffy/specs.nvim"] = {
	opt = true,
	event = "CursorMoved",
	config = get_config("specs"),
}
editor["folke/trouble.nvim"] = {
	opt = true,
	cmd = { "Trouble", "TroubleToggle", "TroubleRefresh" },
	config = get_config("trouble"),
}
editor["nvim-treesitter/nvim-treesitter"] = {
	opt = true,
	run = ":TSUpdate",
	event = "BufRead",
	config = get_config("treesitter"),
}
editor["nvim-treesitter/nvim-treesitter-textobjects"] = {
	opt = true,
	after = "nvim-treesitter",
}
editor["romgrk/nvim-treesitter-context"] = {
	opt = true,
	after = "nvim-treesitter",
}
editor["p00f/nvim-ts-rainbow"] = {
	opt = true,
	after = "nvim-treesitter",
	event = "BufRead",
}
editor["JoosepAlviste/nvim-ts-context-commentstring"] = {
	opt = true,
	after = "nvim-treesitter",
}
editor["abecodes/tabout.nvim"] = {
	opt = true,
	event = "InsertEnter",
	wants = "nvim-treesitter",
	after = "nvim-cmp",
	config = get_config("tabout"),
}

return editor
