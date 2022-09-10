{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.virtual.virtualbox;
in {
  options.modules.desktop.virtual.virtualbox = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    virtualisation.virtualbox = {
      guest.enable = true;
      host = {
        enable = true;
        # enableExtensionPack = true;
      };
    };

    user.extraGroups = ["vboxusers"];
  };
}
