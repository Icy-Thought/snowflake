{ config, lib, pkgs, ... }: {

  xdg.configFile."leftwm/themes/Garden" = {
    "leftwm/config.toml".source = ./config.toml;
    "theme.toml".source = ./theme.toml;
    "leftwm/default-apps.sh".source = ./default-apps.sh;
  };
}
