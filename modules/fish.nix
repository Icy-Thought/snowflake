{ config, pkgs, ... }:

{
  programs.fish = {
    enable = true;
    shellInit = builtins.readFile ../config/fish/config.fish;
    shellAbbrs = {

      # General
      ls     = "exa -l";
      lsa    = "exa -la";
      ytdl   = "youtube-dl";
      bat0   = "upower -i /org/freedesktop/UPower/devices/battery_BAT0";

      # Application-related
      temacs = "emacsclient -t";
      emacs  = "emacsclient -c";
      zoom   = "firejail zoom";

      # VPN
      wup = "systemctl start wg-quick-Akkadian_VPN.service";
      wud = "systemctl stop wg-quick-Akkadian_VPN.service";

      # Git
      g   = "git";
      gc  = "git clone";
      ga  = "git add";
      gaa = "git add -A";
      gcm = "git commit -m";
      gps = "git push";
      gpl = "git pull";
      gs  = "git status";

      # NixOS
      g2nix = "dconf dump / | dconf2nix > ~/git/NixOS-Configurations/home/modules/dconf.nix";
      
    };
  };

  home.packages = with pkgs; [
    starship                                    # Minimal prompt for fish/zsh/bash shell.
  ];

}
