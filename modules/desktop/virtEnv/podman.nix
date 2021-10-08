{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.virtEnv.podman;
in {
  options.modules.desktop.virtEnv.podman.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    virtualisation.podman.enable = false;
    virtualisation.podman.dockerCompat = true;
  };
}
