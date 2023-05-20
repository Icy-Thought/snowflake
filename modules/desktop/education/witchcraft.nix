{
  options,
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib.modules) mkIf;
in {
  options.modules.desktop.education.witchcraft = let
    inherit (lib.options) mkEnableOption;
  in {enable = mkEnableOption "circuit design";};

  config = mkIf config.modules.desktop.education.witchcraft.enable {
    # TODO: OSS packages + configuration.
    user.packages = [pkgs.kicad];
  };
}
