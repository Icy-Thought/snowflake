{ config, pkgs, inputs, ... }:

let
  home = config.home.homeDirectory;

  imports = [
    ./git
    ./bash
    ./fish
    ./scripts
    ./starship
    ./alacritty
    ./kitty
    ./tmux
    ./htop
    ./neofetch
    ./mpd
    ./ncmpcpp
    ./doom-emacs
    ./zathura
    ./fcitx5
    ./ungoogled-chromium
    ../packages/hm-pkgs.nix
  ];

in {
  inherit imports;

  programs = {
    home-manager = {
      enable = true;
      path = "${home}/.nixpkgs/modules/home-manager";
    };

    direnv = {
      enable = true;
      enableFishIntegration = true;
      nix-direnv.enable = true;
    };

  };

  fonts = { fontconfig.enable = true; };

  home.sessionPath = [
    "${home}/.emacs.d/bin"
    "${home}/.cargo/bin"
    "${home}/.local/bin"
    "${home}/go/bin"
  ];

}
