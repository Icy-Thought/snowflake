{ config, lib, pkgs, ... }:
{

  programs.neovim = {
    enable       = true;
    package      = pkgs.neovim-nightly;
    viAlias      = true;
    vimAlias     = true;
    vimdiffAlias = true;
    withPython3  = true; # for plugins
  };

  xdg.configFile."nvim" = {
    source = ../config/nvim;
    recursive = true;
  };

}
