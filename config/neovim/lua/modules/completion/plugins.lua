local completion = {}

function get_config(name)
	return string.format("require(\"modules.completion.config.%s\")", name)
end

function get_lspConf(name)
	return string.format("require(\"modules.completion.lsp.%s\")", name)
end

completion["windwp/nvim-autopairs"] = {
	after = "nvim-cmp",
	config = get_config("autopairs"),
}
completion["neovim/nvim-lspconfig"] = {
	opt = true,
	event = "BufReadPre",
	config = get_lspConf("handlers"),
}
completion["creativenull/efmls-configs-nvim"] = {
	opt = false,
	requires = "neovim/nvim-lspconfig",
}
completion["williamboman/nvim-lsp-installer"] = {
	opt = true,
	after = "nvim-lspconfig",
}
completion["RishabhRD/nvim-lsputils"] = {
	opt = true,
	after = "nvim-lspconfig",
	config = get_lspConf("lsputils"),
}
completion["tami5/lspsaga.nvim"] = { 
	opt = true,
	after = "nvim-lspconfig"
}
completion["ray-x/lsp_signature.nvim"] = {
	opt = true,
	after = "nvim-lspconfig"
}
completion["L3MON4D3/LuaSnip"] = {
	after = "nvim-cmp",
	config = get_config("luasnip"),
	requires = "rafamadriz/friendly-snippets",
}
completion["hrsh7th/nvim-cmp"] = {
	config = get_config("nvim-cmp"),
	event = "InsertEnter",
	requires = {
		{ "lukas-reineke/cmp-under-comparator" },
		{ "saadparwaiz1/cmp_luasnip", after = "LuaSnip" },
		{ "hrsh7th/cmp-nvim-lsp", after = "cmp_luasnip" },
		{ "hrsh7th/cmp-nvim-lua", after = "cmp-nvim-lsp" },
		{ "hrsh7th/cmp-path", after = "cmp-nvim-lua" },
		{ "f3fora/cmp-spell", after = "cmp-path" },
		{ "hrsh7th/cmp-buffer", after = "cmp-spell" },
		{ "kdheepak/cmp-latex-symbols", after = "cmp-buffer" },
	},
}
completion["github/copilot.vim"] = { opt = true, cmd = "Copilot" }

return completion
