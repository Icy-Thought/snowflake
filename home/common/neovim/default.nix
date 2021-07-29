{ config, lib, pkgs, ... }: {

  programs.neovim = {
    enable = true;
    package = pkgs.neovim-unwrapped;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
    withPython3 = true; # for plugins
  };

  xdg.configFile."nvim" = {
    source = ./nvim;
    recursive = true;
    # onChange = "nvim -c PackerSync";
  };

}
