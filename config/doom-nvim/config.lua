-- doom_config - Doom Nvim user configurations file

local M = {}

M.source = debug.getinfo(1, "S").source:sub(2)

M.config = {
  doom = {
    -- Pins plugins to a commit sha to prevent breaking changes
    -- @default = true
    freeze_dependencies = true,

    -- Autosave
    -- @default = false
    autosave = false,

    -- Format on save
    -- @default = false
    fmt_on_save = false,

    -- Disable Vim macros
    -- @default = false
    disable_macros = false,

    -- Use netrw as files explorer
    -- @default = false
    use_netrw = false,

    -- Enable folding
    -- @default = true
    foldenable = true,

    -- Autosave sessions
    -- @default = false
    autosave_sessions = false,

    -- Autoload sessions
    -- @default = false
    autoload_last_session = false,

    -- Enable Swap files
    -- @default = false
    swap_files = false,

    -- Undo Directories
    -- @default = false
    -- WARNING: if you change this to false and you have an undo dir already,
    --          it will REMOVE the undodir (loss of data might take place)
    backup = false,

    -- Enable Line wrapping
    -- @default = false
    line_wrap = false,

    -- Enable Show mode ( -- INSERT --, -- NORMAL --, -- VISUAL -- )
    -- @default = false
    show_mode = false,

    -- Enable scroll off
    -- @default = true, @default scrolloff_amount = 4,
    scrolloff = true,
    scrolloff_amount = 4,

    -- Enable mouse
    -- @default = true
    mouse = true,

    -- Preserve last editing position
    -- @default = false
    preserve_edit_pos = false,

    -- Allow overriding the default Doom Nvim keybinds
    -- @default = true
    allow_default_keymaps_overriding = true,

    -- horizontal split on creating a new file (<Leader>fn)
    -- @default = true
    new_file_split = true,

    -- enable current line highlight
    -- @default = true
    line_highlight = true,

    -- Automatically split right
    -- @default = true
    split_right = true,

    -- Automatically split above
    -- @default = true
    split_below = true,

    -- Use clipboard outside of vim
    -- @default = true
    clipboard = true,

    -- Enable auto comment (current line must be commented)
    -- @default = false
    auto_comment = false,

    -- Show indent lines
    -- @default = true
    show_indent = true,

    -- Expand tabs (spaces || tabs)
    -- @default = true
    expand_tabs = true,

    -- Set numbering
    -- @default = false
    disable_numbering = false,

    -- Set numbering style
    -- @default = true
    relative_num = true,

    -- Enable winwidth
    -- @default = false, @default win_width_nr = 85
    win_width = false,
    win_width_nr = 85,

    -- Enable Highlight on yank
    -- @default = true
    highlight_yank = true,

    -- Enable guicolors
    -- @default = true
    enable_guicolors = true,

    -- Tree explorer on the right
    -- @default = false
    explorer_right = false,

    -- Show hidden files
    -- @default = true
    show_hidden = true,

    -- Checkupdates on start
    -- @default = false
    check_updates = false,

    -- Auto install plugins on launch, useful if you don't want to run
    -- PackerInstall every time you add a new plugin
    -- @default = true
    auto_install_plugins = true,

    -- Disable dashboard status line (does not work perfectly)
    -- @default = true
    dashboard_statline = true,

    -- Show the editing file path in your status line
    -- @default = true
    statusline_show_file_path = true,

    -- Set the keybindings modules that you want to use
    keybinds_modules = {
      -- Core doom keybindings
      core = true,
      -- Movement keybindings, jump between your windows, buffers and code
      movement = true,
      -- Leader keybindings, a bunch of useful keybindings managed by space key
      -- WARNING: disabling this will break which-key plugin if the plugin is enabled
      leader = true,
      -- Completion and snippets keybindings
      completion = true,
    },

    -- sequences used for escaping insert mode
    -- @default = { 'jk', 'kj' }
    escape_sequences = { "jk", "kj" },

    -- Disable or enable Doom autocommands (can break some options = stop working)
    -- @default = false
    disable_autocommands = false,

    -- Enable LSP diagnostics virtual text
    -- @default = false
    enable_lsp_virtual_text = false,

    -- Use floating windows for plugins manager (packer) operations
    -- @default = false
    use_floating_win_packer = false,

    -- Default indent size
    -- @default = 4
    indent = 4,

    -- Set maximum columns (for vertical marker)
    -- @default = 80
    max_columns = 80,

    -- Completion box height
    -- @default = 10
    complete_size = 10,

    -- Completion box transparency
    -- @default = 25
    complete_transparency = 25,

    -- Sidebar sizing
    -- @default = 25
    sidebar_width = 25,

    -- Set the Terminal width
    -- @default = 70
    terminal_width = 70,

    -- Set the Terminal height (all directions)
    -- @default = 20
    terminal_height = 20,

    -- Set Neovim conceal level:
    -- 0 : Disable indentline and show all
    -- 1 : Conceal some functions and show indentlines
    -- 2 : Concealed text is completely hidden unless it has a custom replacement
    --     character defined
    -- 3 : Concealed text is completely hidden
    conceallevel = 0,

    -- Set Doom logging level
    --   · trace
    --   · debug
    --   · info
    --   · warn
    --   · error
    --   · fatal
    -- @default = 'info'
    logging = "info",

    -- Set the Terminal direction
    --	Options: vertical | horizontal | window | float
    -- @default = 'horizontal'
    terminal_direction = "float",

    -- Set undodir (actovated if backup = true)
    -- @default_directory = '~/.config/nvim/undodir'
    undo_dir = "/undodir",

    -- Default colorscheme
    -- @default = doom-one
    colorscheme = "catppuccin",

    -- Background color
    -- @default = dark
    colorscheme_bg = "dark",

    -- Doom One colorscheme settings
    doom_one = {
      -- If the cursor color should be blue
      -- @default = false
      cursor_coloring = false,
      -- If TreeSitter highlighting should be enabled
      -- @default = true
      enable_treesitter = true,
      -- If the comments should be italic
      -- @default = false
      italic_comments = false,
      -- If the telescope plugin window should be colored
      -- @default = true
      telescope_highlights = true,
      -- If built-in Neovim terminal should use doom-one palette
      -- @default = false
      terminal_colors = true,
      -- If Neovim instance should be transparent
      -- @default = false
      transparent_background = false,
    },

    -- Set gui fonts here
    -- @default = "FiraCode Nerd Font", @default font size = 15,
    guifont = "VictorMono Nerd Font",
    guifont_size = "12",

    -- Change Which Key background color
    -- Use hex | color names, eg: Red, Gree, Blue
    -- @default = #202328
    -- whichkey_bg = "#202328",

    -- Set your custom lsp diagnostic symbols below
    lsp_error = "",
    lsp_warn = "",
    lsp_hint = "",
    lsp_info = "",
    lsp_virtual_text = " ",

    -- Set your linters for the programming languages that you use,
    -- see https://github.com/mfussenegger/nvim-lint#available-linters
    linters = {
      c = {},
      cpp = {},
      css = {},
      html = {},
      javascript = {},
      lua = {},
      markdown = {},
      nix = {},
      python = {},
      ruby = {},
      sh = {},
      typescript = {},
    },

    -- Set your dashboard custom colors below
    -- @default = doom emacs' default dashboard colors
    dashboard_custom_colors = {
      header_color = "#586268",
      center_color = "#51afef",
      shortcut_color = "#a9a1e1",
      footer_color = "#586268",
    },

    -- Set your custom dashboard header below
    -- @default = doom emacs' default dashboard header
    dashboard_custom_header = {},
  },

  nvim = {
    -- Set custom Neovim global variables
    -- @default = {}
    -- example:
    --   {
    --     ['sonokai_style'] = 'andromeda',
    --     ['modelineexpr'] = true,
    --   }
    global_variables = {},

    -- Set custom autocommands
    -- @default = {}
    -- example:
    --   augroup_name = {
    --      { 'BufNewFile,BufRead', 'doom_modules.lua', 'set ft=lua'}
    --   }
    autocmds = {},

    -- Set custom key bindings
    -- @default = {}
    -- example:
    --   {
    --      {'n', 'ca', ':Lspsaga code_action<CR>', options}
    --   }
    --
    --   where
    --     'n' is the map scope
    --     'ca' is the map activator
    --     ':Lspsaga ...' is the command to be executed
    --     options is a Lua table containing the mapping options, e.g.
    --     { silent = true }, see ':h map-arguments'.
    mappings = {},

    -- Set custom commands
    -- @default = {}
    -- example:
    --   {
    --      'echo "Hello, custom commands!"'
    --   }
    commands = {},

    -- Set custom functions
    -- @default = {}
    -- example:
    --   {
    --      {
    --         hello_custom_func = function()
    --           print("Hello, custom functions!")
    --         end,
    --         -- If the function should be ran on neovim launch or if it should
    --         -- be a global function accesible from anywhere
    --         run_on_start = false,
    --      },
    --   }
    functions = {},

    -- Set custom options
    -- @default = {}
    -- example:
    --   {
    --      ['shiftwidth'] = 4
    --   }
    options = {},
  },
}

return M

-- vim: sw=2 sts=2 ts=2 fdm=indent noexpandtab
