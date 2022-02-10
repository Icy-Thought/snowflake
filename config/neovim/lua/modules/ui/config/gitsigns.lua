local gitsigns = require("gitsigns")

gitsigns.setup({
	signs = {
		add = { 
			hl = "GitSignsAdd", 
			text = "▎", 
			numhl = "GitSignsAddNr", 
			linehl = "GitSignsAddLn",
		},
		change = { 
			hl = "GitSignsChange",
			text = "▎",
			numhl = "GitSignsChangeNr",
			linehl = "GitSignsChangeLn",
		},
		delete = {
			hl = "GitSignsDelete",
			text = "契",
			numhl = "GitSignsDeleteNr",
			linehl = "GitSignsDeleteLn",
		},
		topdelete = {
			hl = "GitSignsDelete",
			text = "契",
			numhl = "GitSignsDeleteNr",
			linehl = "GitSignsDeleteLn",
		},
		changedelete = {
			hl = "GitSignsChange",
			text = "▎",
			numhl = "GitSignsChangeNr",
			linehl = "GitSignsChangeLn",
		},
	},
	attach_to_untracked = true,
	current_line_blame = false,
	current_line_blame_opts = {
		virt_text = true,
		virt_text_pos = "eol",
		delay = 1000,
		ignore_whitespace = false,
	},
	current_line_blame_formatter_opts = {
		relative_time = false,
	},
	linehl = false,
	max_file_length = 40000,
	numhl = false,
	sign_priority = 6,
	signcolumn = true,
	status_formatter = nil,
	update_debounce = 100,
	watch_gitdir = { interval = 1000, follow_files = true },
	word_diff = false,
	preview_config = {
		border = "single",
		style = "minimal",
		relative = "cursor",
		row = 0,
		col = 1,
	},
	yadm = {
		enable = false,
	},
})
