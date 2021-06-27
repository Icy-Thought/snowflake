{ config, pkgs, ... }:

let
  gappsPkgs= with pkgs.gnome; [
    geary                                                         # Gnome 2nd E-Mail client.
    zenity                                                        # Display Dialogs.
    polari                                                        # Gnome IRC client.
    meld                                                          # Visual merge/diff tool.
    gnome-boxes                                                   # Remove/Virtual system management.
    gnome-dictionary                                              # To look up mispellings.
    gnome-disk-utility                                            # Manage your disks through gnome.
    gnome-tweak-tool                                              # Advance Gnome setting control.
    gnome-software                                                # Gnome software store.
    gnome-packagekit                                              # Allows installing software on Gnome.
  ];

  gextPkgs = with pkgs.gnomeExtensions; [
    # pop-os-shell                                                # Gnome Tiling Manager.
    gsconnect                                                     # KDE Connect for Gnome-shell.
    user-themes                                                   # Enable Gnome-shell theming.
  ];

  ricePkgs = with pkgs; [
    orchis-theme
    flat-remix-gtk
    whitesur-icon-theme
  ];

in {
  imports = [
    ../../modules/home-manager/gnome/dconf.nix
  ];

  # Enable chrome-gnome-shell in FireFox nightly (mozilla-overlay):
  home = {
    file.".mozilla/native-messaging-hosts/org.gnome.chrome_gnome_shell.json".source = "${pkgs.chrome-gnome-shell}/lib/mozilla/native-messaging-hosts/org.gnome.chrome_gnome_shell.json";

    packages = builtins.concatLists [
      gappsPkgs
      gextPkgs
      ricePkgs
    ];
  };

}
