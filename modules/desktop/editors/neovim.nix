{
  config,
  options,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.editors.neovim;
  nvimDir = "${config.snowflake.configDir}/nvim.d";
  colorscheme = config.modules.themes.neovim.theme;
in {
  options.modules.desktop.editors.neovim = {
    ereshkigal.enable = mkBoolOpt false; # fnl
    agasaya.enable = mkBoolOpt false; # lua
    niflheim.enable = mkBoolOpt false; # lua + nix
  };

  config = mkMerge [
    {
      nixpkgs.overlays = with inputs; [neovim-nightly.overlay];

      environment.shellAliases = {
        vi = "nvim";
        vim = "nvim";
        vimdiff = "nvim -d";
      };
    }

    (mkIf (!cfg.niflheim.enable) {
      user.packages = with pkgs; [
        neovide
        neovim-nightly
        (python310.withPackages (pypkgs: with pypkgs; [pynvim]))
      ];
    })

    (mkIf cfg.ereshkigal.enable {
      # environment.variables.NVIMDIR = "${nvimDir}/ereshkigal";
      modules.develop.lua.fennel.enable = true;

      home.configFile."nvim" = {
        source = "${nvimDir}/ereshkigal";
        recursive = true;
      };
    })

    (mkIf cfg.agasaya.enable {
      # environment.variables.NVIMDIR = "${nvimDir}/agasaya";
      modules.develop.lua.enable = true;

      home.configFile = {
        "nvim" = {
          source = "${nvimDir}/agasaya";
          recursive = true;
        };

        "nvim/init.lua".text = ''
          require("core.packer")
          require("core.settings")
          require("core.neovide")

          require("keymaps.default")
          require("keymaps.which-key")

          -- Apply colorscheme
          vim.cmd("colorscheme ${colorscheme}")

          -- Point nvim to correct sqlite path
          vim.g.sqlite_clib_path = "${pkgs.sqlite.out}/lib/libsqlite3.so"
        '';
      };
    })

    (mkIf cfg.niflheim.enable {
      # environment.variables.NVIMDIR = "${nvimDir}/niflheim";
      modules.develop.lua.enable = true;

      home.programs.neovim = let
        customPlugins =
          pkgs.callPackage "${nvimDir}/niflheim/custom-plugins.nix" pkgs;
        plugins = pkgs.vimPlugins // customPlugins;
      in {
        enable = true;
        package = pkgs.neovim-nightly;
        extraConfig = builtins.concatStringsSep "\n" [
          ''
            lua vim.cmd([[colorscheme ${colorscheme}]])
            luafile ${builtins.toString "${nvimDir}/niflheim/lua/options.lua"}
            luafile ${builtins.toString "${nvimDir}/niflheim/lua/keymaps.lua"}

            lua require('colorizer').setup()
            lua require('gitsigns').setup()
            lua require('neogit').setup()
            lua require('octo').setup()
            lua require('spectre').setup()
            lua require('toggleterm').setup()
            lua require('Comment').setup()
            lua require('trouble').setup()
            lua require('nvim-autopairs').setup()
          ''
        ];
        extraPackages = with pkgs; [texlab vale];
        plugins = pkgs.callPackage "${nvimDir}/niflheim/plugins.nix" plugins;
      };

      home.configFile."nvim/lua/my-snippets" = {
        source = "${nvimDir}/niflheim/my-snippets";
        recursive = true;
      };
    })
  ];
}
