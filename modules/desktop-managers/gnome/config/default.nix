{ config, lib, pkgs, ... }:

let
  envPkgs = with pkgs;
    [
      dconf2nix # Nixify your dconf-settings.
    ];

  gappsPkgs = with pkgs.gnome; [
    geary # Gnome 2nd E-Mail client.
    zenity # Display Dialogs.
    polari # Gnome IRC client.
    meld # Visual merge/diff tool.
    gnome-boxes # Remove/Virtual management.
    gnome-dictionary # Look up mispellings.
    gnome-disk-utility # Manage disks through Gnome.
    gnome-tweak-tool # Advance Gnome setting control.
  ];

  gextPkgs = with pkgs.gnomeExtensions; [
    # pop-os-shell # Gnome Tiling Manager.
    gsconnect # KDE Connect for Gnome-shell.
    user-themes # Enable Gnome-shell theming.
  ];

  ricePkgs = with pkgs; [ orchis-theme flat-remix-gnome whitesur-icon-theme ];

in {
  imports = [
    # ./appearance
    ./dconf
  ];

  home.packages = envPkgs ++ gappsPkgs ++ gextPkgs ++ ricePkgs;

  # Enable chrome-gnome-shell in FireFox nightly (mozilla-overlay):
  home.file.".mozilla/native-messaging-hosts/org.gnome.chrome_gnome_shell.json" =
    {
      source =
        "${pkgs.chrome-gnome-shell}/lib/mozilla/native-messaging-hosts/org.gnome.chrome_gnome_shell.json";
    };
}
