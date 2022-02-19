{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.appliances.editors.neovim;
  configDir = config.snowflake.configDir;
in {
  options.modules.desktop.appliances.editors.neovim = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      # Neovim (TUI + GUI) dependencies:
      unstable.neovim

      # LSP Dependencies:
      sumneko-lua-language-server

      # Extra Dependencies:
      stylua
      editorconfig-core-c
    ];

    environment.shellAliases = {
      vi = "nvim";
      vim = "nvim";
      vimdiff = "nvim -d";
    };

    # TODO: clone doom-nvim -> ln -s config files to configDir/doom-nvim
    # (temporary) ln -s settings ~/.config/nvim/lua 
  };
}
