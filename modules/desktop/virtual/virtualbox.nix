{ options, config, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
  inherit (lib.my) mkBoolOpt;
in {
  options.modules.desktop.virtual.virtualbox = { enable = mkBoolOpt false; };

  config = mkIf config.modules.desktop.virtual.virtualbox.enable {
    virtualisation.virtualbox = {
      guest.enable = true;
      host.enable = true;
      # host.enableExtensionPack = true;
    };

    user.extraGroups = [ "vboxusers" ];
  };
}
