{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.virtEnv.vbox;
in {
  options.modules.desktop.virtEnv.vbox.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    virtualisation.virtualbox.host.enable = true;
    # virtualisation.virtualbox.host.enableExtensionPack = true;

    user.extraGroups = [ "vboxusers" ];
  };
}
