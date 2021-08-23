{ config, lib, pkgs, ... }: {

  # QT -> GTK
  qt = {
    enable = true;
    platformTheme = "gtk";
    style.name = "gtk2";
  };

}
