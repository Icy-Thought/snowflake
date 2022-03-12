local lualine = prequire('lualine')

lualine.setup({
    options = {
        icons_enabled = true,
        theme = 'auto',
        section_separators = {"", ""},
        component_separators = {"│", "│"},
        disabled_filetypes = {},
        always_divide_middle = true,
    },
    sections = {
        lualine_a = {'mode'},
        lualine_b = {
            {'branch', icon = ""},
            {
                'diff',
                colored = true,
                diff_color = {
                    added    = { fg = '#28A745' },
                    modified = { fg = '#DBAB09' },
                    removed  = { fg = '#D73A49' }
                },
                symbols = {
                    added    = " ",
                    modified = " ",
                    removed  = " "
                }
            }
        },
        lualine_c = {},
        lualine_x = {
            {
                'diagnostics',
                sources =  {'nvim_lsp'},
                sections = {'error', 'warn', 'info', 'hint'},
                diagnostics_color = {
                    error = { fg = '#AF0000' },
                    warn  = { fg = '#D75F00' },
                    info  = { fg = '#0087AF' },
                    hint  = { fg = '#008700' }
                },
                symbols = {
                    error = " ",
                    warn  = " ",
                    info  = " ",
                    hint  = " "
                }
            }
        },
        lualine_y = {},
        lualine_z = {}
    },
    inactive_sections = {
        lualine_a = {},
        lualine_b = {},
        lualine_c = {'filename'},
        lualine_x = {'location'},
        lualine_y = {},
        lualine_z = {}
    },
    tabline = {},
    extensions = {}
})
