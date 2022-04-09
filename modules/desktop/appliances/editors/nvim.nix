{ config, options, lib, pkgs, inputs, ... }:

with lib;
with lib.my;
let
  acs = config.modules.themes.active;
  cfg = config.modules.desktop.appliances.editors.nvim;
  configDir = config.snowflake.configDir;
  nvimDir = "${configDir}/nvim.d/niflheim";
    in
    {
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

    home.configFile = {
      "stylua/stylua.toml".source =
        "${configDir}/formatters/stylua.toml";
      "nvim/lua/my-snippets" = {
        source = "${nvimDir}/my-snippets";
        recursive = true;
      };
    };

    homeManager.programs.neovim =
      let
        customPlugins = pkgs.callPackage "${nvimDir}/custom-plugins.nix" pkgs;
        plugins = pkgs.vimPlugins // customPlugins;
      in
      {
        enable = true;
        package = pkgs.neovim-nightly;
        extraConfig = builtins.concatStringsSep "\n" [
          ''
            lua vim.cmd([[colorscheme ${acs}]])
            luafile ${builtins.toString "${nvimDir}/lua/options.lua"}
            luafile ${builtins.toString "${nvimDir}/lua/keymaps.lua"}
          ''
        ];
        extraPackages = with pkgs; [
          texlab
          stylua
          sumneko-lua-language-server
          vale
        ];
        plugins = pkgs.callPackage "${nvimDir}/plugins.nix" plugins;
      };
  };
  }
