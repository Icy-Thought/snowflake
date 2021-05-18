{ config, pkgs, ... }:

{
  programs.fish = {
    enable = true;
    shellInit = builtins.readFile ../config/config.fish;
    shellAliases = {

      # General
      ls     = "exa -l";
      lsa    = "exa -la";
      ytdl   = "youtube-dl";

      # Application-related
      temacs = "emacsclient -t";
      emacs  = "emacsclient -c";
      zoom   = "firejail zoom";

      # Fish
      fshcfg = "nvim ~/.config/fish/config.fish";
      bat0   = "upower -i /org/freedesktop/UPower/devices/battery_BAT0";
      
      # Git
      g   = "git";
      gc  = "git clone";
      ga  = "git add";
      gaa = "git add -A";
      gcm = "git commit -m";
      gps = "git push";
      gpl = "git pull";
      gs  = "git status";
      
      # VPN
      wup = "wg-quick up akkadian_vpn";
      wud = "wg-quick down akkadian_vpn";
    };
  };

  home.packages = with pkgs; [

    # Prompt
    starship
  ];

}
