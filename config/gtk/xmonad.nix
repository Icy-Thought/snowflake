{ config, pkgs, ... }: {

  gtk = {
    enable = true;

    iconTheme = {
      package = pkgs.whitesur-icon-theme;
      name = "WhiteSur-dark";
    };

    theme = {
      package = pkgs.orchis-theme;
      name = "Orchis-dark-compact";
    };
  };

 }
