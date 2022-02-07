local M = {}

function M.get_color(hlgroup, attr)
  return vim.fn.synIDattr(vim.fn.synIDtrans(vim.fn.hlID(hlgroup)), attr, 'gui')
  -- return vim.api.nvim_get_hl_by_name(hlgroup, true)[attr]
end

vim.opt.background = "dark"

function M.catppuccin()
    require'catppuccin'.setup{
        transparent_background = false,
        term_colors = false,
        styles = {
            comments = "italic",
			      functions = "italic,bold",
            keywords = "italic",
            strings = "NONE",
            variables = "NONE",
        },
        integrations = {
            treesitter = true,
            native_lsp = {
                enabled = true,
                virtual_text = {
                    errors = "italic",
                    hints = "italic",
                    warnings = "italic",
                    information = "italic",
                },
                underlines = {
                    errors = "undercurl",
                    hints = "undercurl",
                    warnings = "undercurl",
                    information = "undercurl",
                },
            },
            lsp_trouble = true,
            lsp_saga = false,
            gitgutter = false,
            gitsigns = true,
            telescope = true,
            nvimtree = {
                enabled = true,
                show_root = false,
            },
            which_key = false,
            indent_blankline = {
                enabled = true,
                colored_indent_levels = false,
            },
            dashboard = false,
            neogit = false,
            vim_sneak = false,
            fern = false,
            barbar = false,
            bufferline = true,
            markdown = true,
            lightspeed = false,
            ts_rainbow = false,
            hop = true,
        },
    }
    vim.cmd('colorscheme catppuccin')
end

function M.overrides()
    vim.fn.sign_define('DiagnosticSignError', { text = "" , texthl= 'DiagnosticSignError'})
    vim.fn.sign_define('DiagnosticSignWarn', { text = "", texthl= 'DiagnosticSignWarn'})
    vim.fn.sign_define('DiagnosticSignInfo', { text = "", texthl= 'DiagnosticSignInfo'})
    vim.fn.sign_define('DiagnosticSignHint', { text = "" , texthl= 'DiagnosticSignHint'})
end

return M
