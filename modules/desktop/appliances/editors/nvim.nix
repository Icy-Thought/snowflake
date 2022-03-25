{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let
  acs = config.modules.themes.active;
  cfg = config.modules.desktop.appliances.editors.nvim;
  configDir = "${config.snowflake.configDir}/nvim";
in {
  options.modules.desktop.appliances.editors.nvim = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    environment.shellAliases = {
      vi = "nvim";
      vim = "nvim";
      vimdiff = "nvim -d";
    };

    home.configFile."stylua/stylua.toml".source =
      "${config.snowflake.configDir}/formatters/stylua.toml";

    homeManager.programs.neovim = let
      readFile = file: ext:
        builtins.readFile ("${configDir}" + "/${file}.${ext}");
      readLuaFile = file: (readFile file "lua");
      plugWithLuaCfg = plugin: {
        inherit plugin;
        type = "lua";
        config = readLuaFile "lua/modules/${plugin.pname}/init";
      };
    in {
      enable = true;
      package = pkgs.unstable.neovim-unwrapped;
      extraConfig = builtins.concatStringsSep "\n" [''
        lua vim.cmd([[colorscheme ${acs}]])
        luafile ${builtins.toString "${configDir}/lua/core/options.lua"}
        luafile ${builtins.toString "${configDir}/lua/keymaps/init.lua"}
      ''];

      extraPackages = with pkgs; [
        # extraPkgs
        lazygit
        ## lsp: lua
        stylua
        sumneko-lua-language-server
      ];

      plugins = let
        customPlugins = {
          nvim-lsp-installer = pkgs.vimUtils.buildVimPlugin rec {
            pname = "nvim-lsp-installer";
            version = "2022-03-11";
            src = pkgs.fetchFromGitHub {
              owner = "williamboman";
              repo = pname;
              rev = "a049c201279c7e48e1a3a68acb5a15a306671551";
              sha256 = "ieZ/2n7NXpJuS094gSArX6s5UGTKIlG36ZZNOdMSyIo=";
            };
            dontBuild = true;
          };
          fzy-lua-native = pkgs.vimUtils.buildVimPlugin rec {
            pname = "fzy-lua-native";
            version = "2021-08-02";
            src = pkgs.fetchFromGitHub {
              owner = "romgrk";
              repo = pname;
              rev = "aa00feb01128c4d279c8471898e15898e75d5df5";
              sha256 = "JTldjJz5XnG9bgPfAYvjUaHYm0PtJ0MiDBQ9DfjSTQ8=";
            };
            dontBuild = true;
          };
        };
      in with pkgs.vimPlugins // customPlugins; [
        # UI-related
        (plugWithLuaCfg dashboard-nvim)
        (plugWithLuaCfg nvim-colorizer-lua)
        (plugWithLuaCfg gitsigns-nvim)
        (plugWithLuaCfg nvim-web-devicons)
        (plugWithLuaCfg lualine-nvim)
        (plugWithLuaCfg bufferline-nvim)
        (plugWithLuaCfg which-key-nvim)
        (plugWithLuaCfg wilder-nvim)

        # Git
        (plugWithLuaCfg octo-nvim)
        lazygit-nvim

        # Behaviour
        (plugWithLuaCfg nvim-spectre)
        (plugWithLuaCfg telescope-nvim)
        telescope-file-browser-nvim
        telescope-frecency-nvim
        telescope-fzf-native-nvim
        fzy-lua-native
        (plugWithLuaCfg project-nvim)
        (plugWithLuaCfg toggleterm-nvim)

        # Editor
        (plugWithLuaCfg auto-session)
        (plugWithLuaCfg comment-nvim)
        (plugWithLuaCfg copilot-vim)
        (plugWithLuaCfg indent-blankline-nvim)
        (plugWithLuaCfg nvim-autopairs)
        (plugWithLuaCfg nvim-tree-lua)
        (plugWithLuaCfg todo-comments-nvim)
        (plugWithLuaCfg markdown-preview-nvim)

        # LSP-related
        (plugWithLuaCfg nvim-treesitter)
        nvim-ts-context-commentstring
        # (plugWithLuaCfg conjure) <- Enable once it support haskell..
        (plugWithLuaCfg null-ls-nvim)
        (plugWithLuaCfg trouble-nvim)
        (plugWithLuaCfg nvim-lspconfig)
        (plugWithLuaCfg nvim-lsp-installer)

        ## CMP
        (plugWithLuaCfg nvim-cmp)
        cmp-nvim-lsp
        cmp-buffer
        cmp-path
        cmp-cmdline
        cmp_luasnip

        ## Snippets
        luasnip
        friendly-snippets
      ];
    };
  };
}
