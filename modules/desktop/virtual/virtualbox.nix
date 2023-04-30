{ options, config, lib, pkgs, ... }:

let inherit (lib.modules) mkIf;
in {
  options.modules.desktop.virtual.virtualbox =
    let inherit (lib.options) mkEnableOption;
    in { enable = mkEnableOption false; };

  config = mkIf config.modules.desktop.virtual.virtualbox.enable {
    virtualisation.virtualbox = {
      guest.enable = true;
      host.enable = true;
      # host.enableExtensionPack = true;
    };

    user.extraGroups = [ "vboxusers" ];
  };
}
