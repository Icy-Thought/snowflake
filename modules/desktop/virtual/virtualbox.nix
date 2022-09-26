{ options
, config
, lib
, pkgs
, ...
}:
with lib;
with lib.my; {
  options.modules.desktop.virtual.virtualbox = {
    enable = mkBoolOpt false;
  };

  config = mkIf config.modules.desktop.virtual.virtualbox.enable {
    virtualisation.virtualbox = {
      guest.enable = true;
      host.enable = true;
      # host.enableExtensionPack = true;
    };

    user.extraGroups = [ "vboxusers" ];
  };
}
