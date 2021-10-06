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
    ./doom-emacs
    ./zathura
    ./fcitx5
    ./ungoogled-chromium
    ../packages/hm-pkgs.nix
  ];

in {
  inherit imports;

  fonts.fontconfig.enable = true;

  programs.gnupg.agent.enable = true;
  programs.adb.enable = true;
  users.extraGroups = [ "adbusers" ];

  programs.direnv.enable = true;
  programs.direnv.enableFishIntegration = true;
  programs.direnv.nix-direnv.enable = true;

  home.sessionPath = [
    "${home}/.emacs.d/bin"
    "${home}/.cargo/bin"
    "${home}/.local/bin"
    "${home}/go/bin"
  ];

}
