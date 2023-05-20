{
  options,
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib.modules) mkIf;
in {
  options.modules.virtual.virtualbox = let
    inherit (lib.options) mkEnableOption;
  in {enable = mkEnableOption "cross-platform virtualization";};

  config = mkIf config.modules.virtual.virtualbox.enable {
    virtualisation.virtualbox = {
      guest.enable = true;
      host.enable = true;
      # host.enableExtensionPack = true;
    };

    user.extraGroups = ["vboxusers"];
  };
}
