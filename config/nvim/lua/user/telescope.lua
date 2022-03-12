local telescope = prequire('telescope')

telescope.setup({
    defaults = {
        prompt_prefix = "   ",
        selection_caret = " ",
        vimgrep_arguments = {
            "rg",
            "--color=never",
            "--no-heading",
            "--with-filename",
            "--line-number",
            "--column",
            "--smart-case",
        },
        layout_config = {
            horizontal = {
                prompt_position = "bottom",
                preview_width = 0.55,
                results_width = 0.8,
            },
            vertical = { mirror = false },
            width = 0.87,
            height = 0.80,
            preview_cutoff = 120,
        },
        border = {},
        borderchars = { "─", "│", "─", "│", "╭", "╮", "╯", "╰" },
        buffer_previewer_maker = require("telescope.previewers").buffer_previewer_maker,
        color_devicons = true,
        file_ignore_patterns = { "node_modules" },
        file_previewer = require("telescope.previewers").vim_buffer_cat.new,
        file_sorter = require("telescope.sorters").get_fuzzy_file,
        generic_sorter = require("telescope.sorters").get_generic_fuzzy_sorter,
        grep_previewer = require("telescope.previewers").vim_buffer_vimgrep.new,
        path_display = { "absolute" },
        qflist_previewer = require("telescope.previewers").vim_buffer_qflist.new,
        use_less = true,
        winblend = 0,
    },
    extensions = {
        fzf = {
            fuzzy = false,
            override_generic_sorter = true,
            override_file_sorter = true,
            case_mode = "smart_case",
        },
        frecency = {
            show_scores = true,
            show_unindexed = true,
            ignore_patterns = { "*.git/*", "*/tmp/*" },
        },
    }
})

telescope.load_extension("fzf")
telescope.load_extension("frecency")
