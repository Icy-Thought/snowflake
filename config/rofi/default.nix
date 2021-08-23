{ config, pkgs, ... }: {

  home.packages = with pkgs; [ rofi-systemd ];

  programs.rofi = {
    enable = true;
    theme = ./launcher/style-12.rasi;

    extraConfig = {
      font = "Iosevka 11";
      show-icons = true;
      icon-theme = "Whitesur-dark";
      terminal = "alacritty";
      display-drun = "λ";
      drun-display-format = "{name}";
      disable-history = false;
      fullscreen = false;
      hide-scrollbar = true;
      sidebar-mode = false;
    };

  };

}
