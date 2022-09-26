{ options
, config
, lib
, pkgs
, ...
}:
with lib;
with lib.my; {
  options.modules.desktop.philomath.electroGears = {
    enable = mkBoolOpt false;
  };

  config = mkIf config.modules.desktop.philomath.electroGears.enable {
    # TODO: OSS packages + configuration.
    user.packages = with pkgs; [ kicad ];
  };
}
