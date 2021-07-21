{ config, lib, pkgs, ... }: {

  xdg.configFile."leftwm/config.toml".source = ./config.toml;
  xdg.configFile."leftwm/themes/garden/theme.toml".source = ./theme.toml;
  xdg.configFile."leftwm/default-apps.sh".source = ./default-apps.sh;
}
