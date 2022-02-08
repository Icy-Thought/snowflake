local M = {}

function M.config()
	local snip_status_ok, luasnip = pcall(require, "luasnip")
	if not snip_status_ok then
		return
	end

	luasnip.config.set_config = {
		history = true,
		updateevents = "TextChanged,TextChangedI",
	}

	require("luasnip/loaders/from_vscode").lazy_load()
end

return M
