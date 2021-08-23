{ config, pkgs, ... }: {

  gtk = {
    enable = true;

    font = {
      name = "JetBrainsMonoMedium Nerd Font";
      size = 10;
    };

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
