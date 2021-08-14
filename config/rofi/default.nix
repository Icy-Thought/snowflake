{ config, pkgs, ... }: {

  programs.rofi = {
    enable = true;
    plugins = with pkgs; [ rofi-systemd ];
    theme = ./launcher/blurry.rasi;

    extraConfig = {
      font = "Noto Sans 10";
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
