local status_ok, alpha = pcall(require, "alpha")
if not status_ok then
	return
end

local dashboard = require("alpha.themes.dashboard")
dashboard.section.header.val = {
  [[                                                    ]],
  [[ ███╗   ██╗███████╗ ██████╗ ██╗   ██╗██╗███╗   ███╗ ]],
  [[ ████╗  ██║██╔════╝██╔═══██╗██║   ██║██║████╗ ████║ ]],
  [[ ██╔██╗ ██║█████╗  ██║   ██║██║   ██║██║██╔████╔██║ ]],
  [[ ██║╚██╗██║██╔══╝  ██║   ██║╚██╗ ██╔╝██║██║╚██╔╝██║ ]],
  [[ ██║ ╚████║███████╗╚██████╔╝ ╚████╔╝ ██║██║ ╚═╝ ██║ ]],
  [[ ╚═╝  ╚═══╝╚══════╝ ╚═════╝   ╚═══╝  ╚═╝╚═╝     ╚═╝ ]],
}

dashboard.section.buttons.val = {
	dashboard.button("SPC f e", "  New file", ":ene <BAR> startinsert <CR>"),
	dashboard.button("SPC f f", "  Find file", ":Telescope find_files <CR>"),
	dashboard.button("SPC f d", "  Find project", ":Telescope projects <CR>"),
	dashboard.button("SPC f r", "  Recently used files", ":Telescope oldfiles <CR>"),
	dashboard.button("SPC f t", "  Find text", ":Telescope live_grep <CR>"),
	dashboard.button("SPC f p", "  Configuration", ":e ~/.config/nvim/init.lua <CR>"),
	dashboard.button("SPC q q", "  Quit Neovim", ":qa<CR>"),
}

local function footer()
	return "Obeying Icy-Thought's Command!"
end

dashboard.section.footer.val = footer()

dashboard.section.footer.opts.hl = "Type"
dashboard.section.header.opts.hl = "Include"
dashboard.section.buttons.opts.hl = "Keyword"

dashboard.opts.opts.noautocmd = true
-- vim.cmd([[autocmd User AlphaReady echo 'ready']])
alpha.setup(dashboard.opts)
