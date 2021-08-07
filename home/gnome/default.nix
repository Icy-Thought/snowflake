{ config, lib, pkgs, ... }:

let
  genvPkgs = [
    pkgs.dconf2nix # Nixify your dconf-settings.
    pkgs.gnome.zenity # Display Dialogs.
    pkgs.gnome.polari # Gnome IRC client.
    pkgs.gnome.meld # Visual merge/diff tool.
    pkgs.gnome.gnome-boxes # Remove/Virtual management.
    pkgs.gnome.gnome-dictionary # Look up mispellings.
    pkgs.gnome.gnome-disk-utility # Manage disks through Gnome.
    pkgs.gnome.gnome-tweak-tool # Advance Gnome setting control.
  ];

  gextPkgs = [
    # pkgs.gnomeExtensions.pop-os-shell # Gnome Tiling Manager.
    pkgs.gnomeExtensions.gsconnect # KDE Connect for Gnome-shell.
    pkgs.gnomeExtensions.user-themes # Enable Gnome-shell theming.
  ];

  ricePkgs =
    [ pkgs.orchis-theme pkgs.flat-remix-gnome pkgs.whitesur-icon-theme ];

in {
  imports = [
    # ./gtk
    ./dconf
  ];

  home = {
    sessionVariables = { MOZ_ENABLE_WAYLAND = 1; };
    packages = genvPkgs ++ gextPkgs ++ ricePkgs;
  };

  # Enable chrome-gnome-shell in FireFox nightly (mozilla-overlay):
  home.file.".mozilla/native-messaging-hosts/org.gnome.chrome_gnome_shell.json" =
    {
      source =
        "${pkgs.chrome-gnome-shell}/lib/mozilla/native-messaging-hosts/org.gnome.chrome_gnome_shell.json";
    };
}
