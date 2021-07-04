{ config, pkgs, ... }: {

  imports = [ ./dconf.nix ];

  gtk = {
    enable = true;

    iconTheme = {
      package = pkgs.whitesur-icon-theme;
      name = "Whitesur-dark";
    };

    theme = {
      package = pkgs.flat-remix-gnome;
      name = "Flat-Remix-Green";
    };

    gtk3.extraConfig = {
      gtk-icon-theme-name = "Whitesur-dark";
      gtk-theme-name = "Flat-Remix-Green";
      gtk-application-prefer-dark-theme = 1;
    };
  };

  services.gnome-keyring.enable = true;
}
