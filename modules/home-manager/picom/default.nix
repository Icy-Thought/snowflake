{ config, lib, pkgs, ... }:

{
  services.picom = {
    enable = true;
    backend = "glx";
    # experimentalBackends = true;
    # package = pkgs.nur.repos.reedrw.picom-next-ibhagwan;
  };

}
