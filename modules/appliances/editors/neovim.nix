{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.appliances.editors.neovim;
  configDir = config.snowflake.configDir;
in {
  options.modules.appliances.editors.neovim = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      # Neovim (TUI + GUI) dependencies:
      unstable.neovim
      unstable.neovide

      # LSP-Dependencies:
      stylua
      nodePackages.prettier

      # Extra Dependencies:
      editorconfig-core-c
    ];

    environment.shellAliases = {
      vi = "nvim";
      vim = "nvim";
      vimdiff = "nvim -d";
    };

    # TODO: style neovim first -> make a module for installing LunarVim
    # + also, setup neovide properly without dpi issues.
    # + proper shellAliases when lvim.enable = true; such as lv alias.
  };
}
