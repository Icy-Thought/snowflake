{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let
  acs = config.modules.themes.active;
  cfg = config.modules.desktop.appliances.editors.neovim;
  configDir = "${config.snowflake.configDir}/nvim/lua/user";
  lspDir = "${configDir}/lsp";
in {
  options.modules.desktop.appliances.editors.neovim = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    environment.shellAliases = {
      vi = "nvim";
      vim = "nvim";
      vimdiff = "nvim -d";
    };

    homeManager.programs.neovim = {
      enable = true;
      package = pkgs.unstable.neovim-unwrapped;
      extraConfig = builtins.concatStringsSep "\n" [''
        lua vim.cmd([[colorscheme ${acs}]])
        luafile ${builtins.toString "${configDir}/options.lua"}
        luafile ${builtins.toString "${configDir}/keymaps.lua"}
      ''];

      extraPackages = with pkgs; [
        sumneko-lua-language-server
        editorconfig-core-c
      ];

      plugins = with pkgs.vimPlugins;
        let
          nvim-lsp-installer = pkgs.vimUtils.buildVimPlugin rec {
            name = "nvim-lsp-installer";
            src = pkgs.fetchFromGitHub {
              owner = "williamboman";
              repo = name;
              rev = "a049c201279c7e48e1a3a68acb5a15a306671551";
              sha256 = "ieZ/2n7NXpJuS094gSArX6s5UGTKIlG36ZZNOdMSyIo=";
            };
            dontBuild = true;
          };
          fzy-lua-native = pkgs.vimUtils.buildVimPlugin rec {
            name = "fzy-lua-native";
            src = pkgs.fetchFromGitHub {
              owner = "romgrk";
              repo = name;
              rev = "aa00feb01128c4d279c8471898e15898e75d5df5";
              sha256 = "JTldjJz5XnG9bgPfAYvjUaHYm0PtJ0MiDBQ9DfjSTQ8=";
            };
            dontBuild = true;
          };
        in [
          {
            plugin = auto-session;
            type = "lua";
            config = builtins.readFile "${configDir}/auto-session.lua";
          }
          {
            plugin = bufferline-nvim; # check replacement
            type = "lua";
            config = builtins.readFile "${configDir}/bufferline.lua";
          }
          {
            plugin = comment-nvim;
            type = "lua";
            config = builtins.readFile "${configDir}/comment.lua";
          }
          # { Enable once it support haskell..
          #   plugin = conjure;
          #   type = "lua";
          #   config = builtins.readFile "${configDir}/conjure.lua";
          # }
          {
            plugin = copilot-vim;
            type = "lua";
            config = builtins.readFile "${configDir}/copilot.lua";
          }
          {
            plugin = dashboard-nvim;
            type = "lua";
            config = builtins.readFile "${configDir}/dashboard.lua";
          }
          {
            plugin = gitsigns-nvim;
            type = "lua";
            config = builtins.readFile "${configDir}/gitsigns.lua";
          }
          {
            plugin = impatient-nvim;
            type = "lua";
            config = ''
              require('impatient').enable_profile()
            '';
          }
          {
            plugin = indent-blankline-nvim;
            type = "lua";
            config = builtins.readFile "${configDir}/indent-blankline.lua";
          }
          {
            plugin = lualine-nvim;
            type = "lua";
            config = builtins.readFile "${configDir}/lualine.lua";
          }
          {
            plugin = nvim-autopairs;
            type = "lua";
            config = builtins.readFile "${configDir}/autopairs.lua";
          }
          {
            plugin = nvim-colorizer-lua;
            type = "lua";
            config = builtins.readFile "${configDir}/nvim-colorizer.lua";
          }

          # Compelation (CMP)
          {
            plugin = nvim-cmp;
            type = "lua";
            config = builtins.readFile "${configDir}/nvim-cmp.lua";
          }
          cmp-nvim-lsp
          cmp-buffer
          cmp-path
          cmp-cmdline
          cmp_luasnip

          # Snippet-CMP
          luasnip
          friendly-snippets

          # Language Server Protocol (LSP)
          nvim-lspconfig
          {
            plugin = null-ls-nvim;
            type = "lua";
            config = builtins.readFile "${lspDir}/null-ls.lua";
          }
          {
            plugin = nvim-lsp-installer;
            type = "lua";
            config = builtins.readFile "${lspDir}/lsp-installer.lua";
          }

          {
            plugin = nvim-web-devicons;
            type = "lua";
            config = builtins.readFile "${configDir}/devicons.lua";
          }
          {
            plugin = nvim-tree-lua;
            type = "lua";
            config = builtins.readFile "${configDir}/nvim-tree.lua";
          }
          {
            plugin = nvim-treesitter;
            type = "lua";
            config = builtins.readFile "${configDir}/nvim-treesitter.lua";
          }
          nvim-ts-context-commentstring
          {
            plugin = octo-nvim;
            type = "lua";
            config = builtins.readFile "${configDir}/octo.lua";
          }
          lazygit-nvim
          {
            plugin = project-nvim;
            type = "lua";
            config = builtins.readFile "${configDir}/project.lua";
          }
          {
            plugin = nvim-spectre;
            type = "lua";
            config = builtins.readFile "${configDir}/spectre.lua";
          }
          {
            plugin = telescope-nvim;
            type = "lua";
            config = builtins.readFile "${configDir}/telescope.lua";
          }
          popup-nvim
          plenary-nvim
          telescope-frecency-nvim
          telescope-fzf-native-nvim
          {
            plugin = trouble-nvim;
            type = "lua";
            config = builtins.readFile "${configDir}/trouble.lua";
          }
          {
            plugin = toggleterm-nvim;
            type = "lua";
            config = builtins.readFile "${configDir}/toggle-term.lua";
          }
          {
            plugin = which-key-nvim;
            type = "lua";
            config = builtins.readFile "${configDir}/which-key.lua";
          }
          {
            plugin = wilder-nvim;
            type = "lua";
            config = builtins.readFile "${configDir}/wilder.lua";
          }
          fzy-lua-native
        ];
    };
  };
}
