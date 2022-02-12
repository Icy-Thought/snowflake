local alpha = require("alpha")
local dashboard = require("alpha.themes.dashboard")

dashboard.section.header.val = {
	[[=================     ===============     ===============   ========  ========]],
	[[\\ . . . . . . .\\   //. . . . . . .\\   //. . . . . . .\\  \\. . .\\// . . //]],
	[[||. . ._____. . .|| ||. . ._____. . .|| ||. . ._____. . .|| || . . .\/ . . .||]],
	[[|| . .||   ||. . || || . .||   ||. . || || . .||   ||. . || ||. . . . . . . ||]],
	[[||. . ||   || . .|| ||. . ||   || . .|| ||. . ||   || . .|| || . | . . . . .||]],
	[[|| . .||   ||. _-|| ||-_ .||   ||. . || || . .||   ||. _-|| ||-_.|\ . . . . ||]],
	[[||. . ||   ||-'  || ||  `-||   || . .|| ||. . ||   ||-'  || ||  `|\_ . .|. .||]],
	[[|| . _||   ||    || ||    ||   ||_ . || || . _||   ||    || ||   |\ `-_/| . ||]],
	[[||_-' ||  .|/    || ||    \|.  || `-_|| ||_-' ||  .|/    || ||   | \  / |-_.||]],
	[[||    ||_-'      || ||      `-_||    || ||    ||_-'      || ||   | \  / |  `||]],
	[[||    `'         || ||         `'    || ||    `'         || ||   | \  / |   ||]],
	[[||            .===' `===.         .==='.`===.         .===' /==. |  \/  |   ||]],
	[[||         .=='   \_|-_ `===. .==='   _|_   `===. .===' _-|/   `==  \/  |   ||]],
	[[||      .=='    _-'    `-_  `='    _-'   `-_    `='  _-'   `-_  /|  \/  |   ||]],
	[[||   .=='    _-'          '-__\._-'         '-_./__-'         `' |. /|  |   ||]],
	[[||.=='    _-'                                                     `' |  /==.||]],
	[[=='    _-'                        N E O V I M                         \/   `==]],
	[[\   _-'                                                                `-_   /]],
	[[ `''                                                                      ``' ]],
}

dashboard.section.buttons.val = {
	dashboard.button("SPC f f", "  Find File", "DashboardFindFile"),
	dashboard.button("SPC f d", "  Find Project", "Telescope project"),
	dashboard.button("SPC f r", "  Smart File Finder", "Telescope frecency"),
	dashboard.button("SPC f e", "  Recently Edited Files", "DashboardFindHistory"),
	dashboard.button("SPC f p", "  Neovim Configuration", ":e ~/.config/nvim/lua/core/init.lua <CR>"),
	dashboard.button("SPC s c", "	Change Colorscheme", "DashboardChangeColorscheme"),
	dashboard.button("SPC q q", "  Quit Neovim", ":qa<CR>"),
}

local function footer()
	local plug_dir = "$HOME/.local/share/nvim/site/pack/packer/start"
	local loaded_plugins = #vim.fn.globpath(plug_dir, "*", 0, 1)
	return "Neovim loaded " .. loaded_plugins .. " plugins "
end

dashboard.section.footer.val = footer()

dashboard.section.footer.opts.hl = "Type"
dashboard.section.header.opts.hl = "Include"
dashboard.section.buttons.opts.hl = "Keyword"

dashboard.opts.opts.noautocmd = true

alpha.setup(dashboard.opts)
