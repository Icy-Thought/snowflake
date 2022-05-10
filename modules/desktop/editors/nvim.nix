{ config, options, lib, pkgs, inputs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.editors.nvim;
  nvimDir = "${config.snowflake.configDir}/nvim.d";
  active = config.modules.themes.active;
in {
  options.modules.desktop.editors.nvim = {
    enable = mkBoolOpt false;
    fnl.enable = mkBoolOpt false;
    lua.enable = mkBoolOpt false;
  };

  config = mkMerge [
    {
      nixpkgs.overlays = [ inputs.neovim-nightly.overlay ];

      user.packages = with pkgs; [ neovim-nightly ];

      environment = {
        # variables.NVIMDIR = "${configDir}/nvim.d/niflheim";
        shellAliases = {
          vi = "nvim";
          vim = "nvim";
          vimdiff = "nvim -d";
        };
      };
    }

    (mkIf cfg.fnl.enable {
      modules.develop.lua.fennel.enable = true;

      home.configFile."nvim" = {
        source = "${config.snowflake.configDir}/nvim.d/pomelo";
        recursive = true;
      };
    })

    (mkIf cfg.lua.enable {
      modules.develop.lua.enable = true;

      homeManager.programs.neovim = let
        customPlugins =
          pkgs.callPackage "${nvimDir}/niflheim/custom-plugins.nix" pkgs;
        plugins = pkgs.vimPlugins // customPlugins;
      in {
        enable = true;
        extraConfig = builtins.concatStringsSep "\n" [''
          lua vim.cmd([[colorscheme ${active}]])
          luafile ${builtins.toString "${nvimDir}/niflheim/lua/options.lua"}
          luafile ${builtins.toString "${nvimDir}/niflheim/lua/keymaps.lua"}
        ''];
        extraPackages = with pkgs; [ texlab vale ];
        plugins = pkgs.callPackage "${nvimDir}/niflheim/plugins.nix" plugins;
      };

      home.configFile."nvim/lua/my-snippets" = {
        source = "${nvimDir}/niflheim/my-snippets";
        recursive = true;
      };
    })
  ];
}
