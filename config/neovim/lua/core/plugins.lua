local fn = vim.fn

-- Automatically install packer
local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
	PACKER_BOOTSTRAP = fn.system({
		"git",
		"clone",
		"--depth",
		"1",
		"https://github.com/wbthomason/packer.nvim",
		install_path,
	})
	print("Installing packer close and reopen Neovim...")
	vim.cmd([[packadd packer.nvim]])
end

-- Autocommand that reloads neovim whenever you save the plugins.lua file
vim.cmd([[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerSync
  augroup end
]])

-- Use a protected call so we don't error out on first use
local status_ok, packer = pcall(require, "packer")
if not status_ok then
	return
end

-- Have packer use a popup window
packer.init({
	display = {
		open_fn = function()
			return require("packer.util").float({ border = "rounded" })
		end,
	},
})

-- Install your plugins here
return packer.startup(function(use)
	use("wbthomason/packer.nvim")
	use("nvim-lua/plenary.nvim")

	use({
		"lewis6991/impatient.nvim",
		config = require("modules.editor.impatient").config,
	})

	use({
		"ahmedkhalf/project.nvim",
		config = require("modules.editor.project").config,
	})

	-- UI-related:
	use("nvim-lua/popup.nvim")
	use("antoinemadec/FixCursorHold.nvim") -- This is needed to fix lsp doc highlight
	use("kyazdani42/nvim-web-devicons")

	use({
		"goolord/alpha-nvim",
		config = require("modules.ui.alpha").config,
	})

	use({
		"akinsho/bufferline.nvim",
		config = require("modules.ui.bufferline").config,
	})

	use({
		"nvim-lualine/lualine.nvim",
		config = require("modules.ui.lualine").config,
	})

	use({
		"lewis6991/gitsigns.nvim",
		config = require("modules.ui.gitsigns").config,
	})

	use({
		"j-hui/fidget.nvim",
		config = require("modules.ui.fidget").config,
	})

	-- Colorschemes
	use({
		"catppuccin/nvim",
		as = "catppuccin",
		config = require("modules.ui.colors").catppuccin,
	})

	use({
		"rebelot/kanagawa.nvim",
		as = "kanagawa",
		config = require("modules.ui.colors").kanagawa,
	})

	-- Workflow:
	use({
		"windwp/nvim-autopairs",
		config = require("modules.editor.autopairs").config,
	})

	use({
		"lukas-reineke/indent-blankline.nvim",
		config = require("modules.editor.indentline").config,
	})

	use({
		"numToStr/Comment.nvim",
		config = require("modules.editor.comment").config,
	})

	use({
		"folke/trouble.nvim",
		config = require("modules.editor.trouble").config,
	})

	use({
		"abecodes/tabout.nvim",
		wants = { "nvim-treesitter" },
		after = { "nvim-cmp" },
		config = require("modules.ui.tabout").config,
	})

	-- CMP Plugins:
	use({
		"hrsh7th/nvim-cmp",
		config = require("modules.completion.cmp").config,
		requires = {
			{ "lukas-reineke/cmp-under-comparator" },
			{ "saadparwaiz1/cmp_luasnip", after = "LuaSnip" },
			{ "hrsh7th/cmp-nvim-lsp", after = "cmp_luasnip" },
			{ "hrsh7th/cmp-nvim-lua", after = "cmp-nvim-lsp" },
			{ "andersevenrud/cmp-tmux", after = "cmp-nvim-lua" },
			{ "hrsh7th/cmp-path", after = "cmp-tmux" },
			{ "f3fora/cmp-spell", after = "cmp-path" },
			{ "hrsh7th/cmp-buffer", after = "cmp-spell" },
			{ "kdheepak/cmp-latex-symbols", after = "cmp-buffer" },
		},
	})

	-- Snippets:
	use({
		"L3MON4D3/LuaSnip",
		config = require("modules.completion.luasnip").config,
		requires = { "rafamadriz/friendly-snippets" },
	})

	-- LSP:
	use("neovim/nvim-lspconfig")

	use({
		"jose-elias-alvarez/null-ls.nvim",
		config = require("modules.completion.null-ls").config,
	})

	use({
		"williamboman/nvim-lsp-installer",
		config = require("modules.completion.installer").config,
		requires = { "tamago324/nlsp-settings.nvim" },
	})

	-- Toolset:
	use("moll/vim-bbye")

	use({
		"folke/which-key.nvim",
		config = require("modules.toolset.whichkey").config,
	})

	use({
		"kyazdani42/nvim-tree.lua",
		config = require("modules.toolset.nvim-tree").config,
	})

	use({
		"nvim-telescope/telescope.nvim",
		config = require("modules.toolset.telescope").config,
		requires = {
			{ "nvim-lua/popup.nvim" },
			{ "nvim-lua/plenary.nvim" },
		},
	})

	-- Treesitter
	use({
		"nvim-treesitter/nvim-treesitter",
		run = ":TSUpdate",
		config = require("modules.toolset.treesitter").config,
		requires = { "JoosepAlviste/nvim-ts-context-commentstring" },
	})

	-- Automatically set up your configuration after cloning packer.nvim
	-- Put this at the end after all plugins
	if PACKER_BOOTSTRAP then
		require("packer").sync()
	end
end)
