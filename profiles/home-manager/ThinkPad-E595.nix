{ config, pkgs, ... }: {

  imports = [
    ../../modules/desktop-managers/gnome/config
    # ../../modules/window-managers/xmonad/config
  ];
}
