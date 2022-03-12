local bufferline = prequire('bufferline')

bufferline.setup {
    options = {
        numbers = "none",
        close_command = "Bdelete! %d",
        right_mouse_command = "Bdelete! %d",
        left_mouse_command = "buffer %d",
        middle_mouse_command = nil,
        -- NOTE: plugin is designed with this icon in mind -> do not change.
        indicator_icon = '▎',
        buffer_close_icon = '',
        modified_icon = '●',
        close_icon = '',
        left_trunc_marker = '',
        right_trunc_marker = '',
        max_name_length = 18,
        max_prefix_length = 15,
        tab_size = 18,
        diagnostics = false,
        offsets = { { filetype = "NvimTree", text = "", padding = 1 } },
        show_buffer_icons = true,
        show_buffer_close_icons = true,
        show_close_icon = true,
        show_tab_indicators = true,
        persist_buffer_sort = true,
        separator_style = 'thin',
        enforce_regular_tabs = true,
        always_show_bufferline = true,
        sort_by = 'id',
    },
    highlights = {
    fill = {
        guifg = { attribute = "fg", highlight = "#ff0000" },
        guibg = { attribute = "bg", highlight = "TabLine" },
    },
    background = {
        guifg = { attribute = "fg", highlight = "TabLine" },
        guibg = { attribute = "bg", highlight = "TabLine" },
    },
    buffer_visible = {
        guifg = { attribute = "fg", highlight = "TabLine" },
        guibg = { attribute = "bg", highlight = "TabLine" },
    },
    close_button = {
        guifg = { attribute = "fg", highlight = "TabLine" },
        guibg = { attribute = "bg", highlight = "TabLine" },
    },
    close_button_visible = {
        guifg = { attribute = "fg", highlight = "TabLine" },
        guibg = { attribute = "bg", highlight = "TabLine" },
    },
    tab_selected = {
        guifg = { attribute = "fg", highlight = "Normal" },
        guibg = { attribute = "bg", highlight = "Normal" },
    },
    tab = {
        guifg = { attribute = "fg", highlight = "TabLine" },
        guibg = { attribute = "bg", highlight = "TabLine" },
    },
    tab_close = {
        guifg = { attribute = "fg", highlight = "TabLineSel" },
        guibg = { attribute = "bg", highlight = "Normal" },
    },
    duplicate_selected = {
        guifg = { attribute = "fg", highlight = "TabLineSel" },
        guibg = { attribute = "bg", highlight = "TabLineSel" },
        gui = "italic",
    },
    duplicate_visible = {
        guifg = { attribute = "fg", highlight = "TabLine" },
        guibg = { attribute = "bg", highlight = "TabLine" },
        gui = "italic",
    },
    duplicate = {
        guifg = { attribute = "fg", highlight = "TabLine" },
        guibg = { attribute = "bg", highlight = "TabLine" },
        gui = "italic",
    },
    modified = {
        guifg = { attribute = "fg", highlight = "TabLine" },
        guibg = { attribute = "bg", highlight = "TabLine" },
    },
    modified_selected = {
        guifg = { attribute = "fg", highlight = "Normal" },
        guibg = { attribute = "bg", highlight = "Normal" },
    },
    modified_visible = {
        guifg = { attribute = "fg", highlight = "TabLine" },
        guibg = { attribute = "bg", highlight = "TabLine" },
    },
    separator = {
        guifg = { attribute = "bg", highlight = "TabLine" },
        guibg = { attribute = "bg", highlight = "TabLine" },
    },
    separator_selected = {
        guifg = { attribute = "bg", highlight = "Normal" },
        guibg = { attribute = "bg", highlight = "Normal" },
    },
    indicator_selected = {
        guifg = { attribute = "fg", highlight = "LspDiagnosticsDefaultHint" },
        guibg = { attribute = "bg", highlight = "Normal" },
    },
  },
}
