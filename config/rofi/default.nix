{ config, pkgs, ... }: {

  home.packages = with pkgs; [ rofi-systemd ];

  programs.rofi = {
    enable = true;
    theme = ./launcher/style-12.rasi;
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
