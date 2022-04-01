{ config, options, lib, pkgs, inputs, ... }:

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
    nixpkgs.overlays = [ inputs.neovim-nightly.overlay ];

    environment.shellAliases = {
      vi = "nvim";
      vim = "nvim";
      vimdiff = "nvim -d";
    };

    home.configFile."stylua/stylua.toml".source =
      "${config.snowflake.configDir}/formatters/stylua.toml";

    homeManager.programs.neovim = let
      customPlugins = pkgs.callPackage "${configDir}/custom-plugins.nix" pkgs;
      plugins = pkgs.vimPlugins // customPlugins;
    in {
      enable = true;
      package = pkgs.neovim-nightly;
      extraConfig = builtins.concatStringsSep "\n" [''
        lua vim.cmd([[colorscheme ${acs}]])
        luafile ${builtins.toString "${configDir}/lua/options.lua"}
        luafile ${builtins.toString "${configDir}/lua/keymaps.lua"}
      ''];
      extraPackages = with pkgs; [ stylua sumneko-lua-language-server ];
      plugins = pkgs.callPackage "${configDir}/plugins.nix" plugins;
    };
  };
}
