{ config, options, lib, pkgs, inputs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.editors.nvim;
  nvimDir = "${config.snowflake.configDir}/nvim.d/niflheim";
  active = config.modules.themes.active;
in {
  options.modules.desktop.editors.nvim = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    nixpkgs.overlays = [ inputs.neovim-nightly.overlay ];

    modules.develop.lua.enable = true;

    environment.shellAliases = {
      vi = "nvim";
      vim = "nvim";
      vimdiff = "nvim -d";
    };

    home.configFile."nvim/lua/my-snippets" = {
      source = "${nvimDir}/my-snippets";
      recursive = true;
    };

    homeManager.programs.neovim = let
      customPlugins = pkgs.callPackage "${nvimDir}/custom-plugins.nix" pkgs;
      plugins = pkgs.vimPlugins // customPlugins;
    in {
      enable = true;
      package = pkgs.neovim-nightly;
      extraConfig = builtins.concatStringsSep "\n" [''
        lua vim.cmd([[colorscheme ${active}]])
        luafile ${builtins.toString "${nvimDir}/lua/options.lua"}
        luafile ${builtins.toString "${nvimDir}/lua/keymaps.lua"}
      ''];
      extraPackages = with pkgs; [ texlab vale ];
      plugins = pkgs.callPackage "${nvimDir}/plugins.nix" plugins;
    };
  };
}
