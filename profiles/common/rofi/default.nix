{ config, pkgs, ... }: {

  home.packages = with pkgs; [ rofi-systemd ];

  programs.rofi = {
    enable = true;
    theme = ./launcher/ribbon-left.rasi;
    terminal = "${pkgs.kitty}/bin/kitty";

    extraConfig = {
      font = "Iosevka 11";
      show-icons = true;
      icon-theme = "Whitesur-dark";
      display-drun = "λ";
      drun-display-format = "{name}";
      disable-history = false;
      sidebar-mode = false;
    };

  };

}
