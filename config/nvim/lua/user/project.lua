local project = prequire('project_nvim')

project.setup({
    active = true,
    datapath = vim.fn.stdpath("data"),
    detection_methods = { "pattern" },
    exclude_dirs = {},
    ignore_lsp = {},
    manual_mode = false,
    on_config_done = nil,
    patterns = { ".git", "_darcs", ".hg", ".bzr", ".svn", "Makefile", "package.json" },
    show_hidden = false,
    silent_chdir = true,
})

local telescope = prequire('telescope')

telescope.load_extension('projects')
