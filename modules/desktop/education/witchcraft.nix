{ options, config, lib, pkgs, ... }:

with lib; {
  options.modules.desktop.education.witchcraft = {
    enable = mkEnableOption "circuit design";
  };

  config = mkIf config.modules.desktop.education.witchcraft.enable {
    # TODO: OSS packages + configuration.
    user.packages = [ pkgs.kicad ];
  };
}
