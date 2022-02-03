{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.appliances.editors.neovim;
  configDir = config.snowflake.configDir;
in {
  options.modules.appliances.editors.neovim = {
    enable = mkBoolOpt false;
    lunarVim = {
      enable = mkBoolOpt true;
      fromSSH = mkBoolOpt false;
    };
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      # Neovim (TUI + GUI) dependencies:
      unstable.neovim

      # Other dependencies:
      editorconfig-core-c
    ];

    environment.shellAliases = {
      vi = "nvim";
      vim = "nvim";
      vimdiff = "nvim -d";
    };

    # TODO: style neovim first -> make a module for installing lunar vim.
    # init.lunarVim = mkIf cfg.lunarVim.enable ''
    #   if [ -d $HOME/.config/nvim ]; then
    #      ${optionalString cfg.lunarVim.fromSSH ''
    #         git clone git@github.com:LunarVim/LunarVim.git $HOME/.config/nvim
    #      ''}
    #
    #      ${optionalString (cfg.lunarVim.fromSSH == false) ''
    #         git clone https://github.com/LunarVim/LunarVim $HOME/.config/nvim --depth 1
    #      ''}
    #   fi
    # '';

  };
}
