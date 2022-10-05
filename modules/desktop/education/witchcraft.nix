{ options
, config
, lib
, pkgs
, ...
}:
with lib;
with lib.my; {
  options.modules.desktop.education.witchcraft = {
    enable = mkBoolOpt false;
  };

  config = mkIf config.modules.desktop.education.witchcraft.enable {
    # TODO: OSS packages + configuration.
    user.packages = with pkgs; [ kicad ];
  };
}
