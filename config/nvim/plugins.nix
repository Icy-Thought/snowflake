plugins:
let
  readFile = file: ext: builtins.readFile (./. + "/${file}.${ext}");
  readLuaFile = file: (readFile file "lua");
  plugWithLuaCfg = plugin: {
    inherit plugin;
    type = "lua";
    config = readLuaFile "lua/config/${plugin.pname}/init";
  };
in with plugins; [
  # UI
  (plugWithLuaCfg dashboard-nvim)
  (plugWithLuaCfg nvim-colorizer-lua)
  (plugWithLuaCfg gitsigns-nvim)
  (plugWithLuaCfg nvim-web-devicons)
  (plugWithLuaCfg lualine-nvim)
  (plugWithLuaCfg bufferline-nvim)
  (plugWithLuaCfg which-key-nvim)
  (plugWithLuaCfg wilder-nvim)
  fzy-lua-native # wilder dep.

  # Utility
  (plugWithLuaCfg neogit)
  (plugWithLuaCfg octo-nvim)
  (plugWithLuaCfg nvim-spectre)
  (plugWithLuaCfg project-nvim)
  (plugWithLuaCfg telescope-nvim)
  telescope-file-browser-nvim
  telescope-frecency-nvim
  telescope-fzf-native-nvim
  (plugWithLuaCfg toggleterm-nvim)

  # Development
  cmp-nvim-lsp
  cmp-buffer
  cmp-path
  cmp-cmdline
  cmp_luasnip
  luasnip
  friendly-snippets
  (plugWithLuaCfg nvim-treesitter)
  nvim-ts-context-commentstring
  (plugWithLuaCfg null-ls-nvim)
  (plugWithLuaCfg trouble-nvim)
  (plugWithLuaCfg nvim-lspconfig)
  (plugWithLuaCfg nvim-lsp-installer)
  (plugWithLuaCfg nvim-cmp)

  # Miscellaneous
  (plugWithLuaCfg auto-session)
  (plugWithLuaCfg comment-nvim)
  (plugWithLuaCfg copilot-vim)
  (plugWithLuaCfg indent-blankline-nvim)
  (plugWithLuaCfg nvim-autopairs)
  (plugWithLuaCfg nvim-tree-lua)
  (plugWithLuaCfg markdown-preview-nvim)
  (plugWithLuaCfg todo-comments-nvim)
]
