{ options, config, lib, pkgs, ... }:

let
  inherit (lib) attrValues mkIf;
  inherit (lib.my) mkBoolOpt;
in {
  options.modules.desktop.virtual.docker = { enable = mkBoolOpt false; };

  config = mkIf config.modules.desktop.virtual.docker.enable {
    user.packages = attrValues ({ inherit (pkgs) docker docker-compose; });

    env = {
      DOCKER_CONFIG = "$XDG_CONFIG_HOME/docker";
      MACHINE_STORAGE_PATH = "$XDG_DATA_HOME/docker/machine";
    };

    user.extraGroups = [ "docker" ];

    virtualisation = {
      docker = {
        enable = true;
        autoPrune.enable = true;
        enableOnBoot = false;
        # listenOptions = [];
      };
    };
  };
}
