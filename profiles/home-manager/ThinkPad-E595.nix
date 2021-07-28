{ config, pkgs, ... }: {

  imports = [
    ../../modules/desktop-managers/gnome/config
    ../../modules/window-managers/xmonad/config
    # ../../modules/window-managers/leftwm/config
  ];
}
