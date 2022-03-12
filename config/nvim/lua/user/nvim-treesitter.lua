local configs = prequire('nvim-treesitter.configs')

configs.setup {
    ensure_installed = "maintained",
    sync_install = false,
    ignore_install = {},
    autopairs = {
        enable = true,
    },
    highlight = {
        enable = true,
        disable = { "" },
        additional_vim_regex_highlighting = false,
    },
    indent = {
        enable = true,
        disable = {},
    },
    context_commentstring = {
        enable = true,
        enable_autocmd = false,
    },
}
