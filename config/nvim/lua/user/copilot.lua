-- Have copilot play nice with nvim-cmp.
vim.g.copilot_no_tab_map = true
vim.g.copilot_assume_mapped = true
vim.g.copilot_tab_fallback = ""
vim.g.copilot_filetypes = {
    ["*"] = false,
    python = true,
    lua = true,
    rust = true,
    html = true;
    javascript = true,
}
