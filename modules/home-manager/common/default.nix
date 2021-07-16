{ config, lib, pkgs, ... }:

{
  imports = [
    ./git
    ./fish
    ./alacritty
    ./tmux
    ./emacs
    ./neovim
    ./zathura
    ./mpd
    ./ncmpcpp
  ];

}
