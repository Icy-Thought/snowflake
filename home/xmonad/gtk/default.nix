{ config, pkgs, ... }: {

  gtk = {
    enable = true;

    iconTheme = {
      package = pkgs.whitesur-icon-theme;
      name = "WhiteSur-dark";
    };

    theme = {
      package = pkgs.orchis-theme;
      name = "orchis-dark-compact";
    };

    gtk3.extraConfig = {
      gtk-icon-theme-name = "WhiteSur-dark";
      gtk-theme-name = "orchis-dark-compact";
      gtk-application-prefer-dark-theme = 1;
    };
  };

}
