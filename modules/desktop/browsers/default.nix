{ options
, config
, lib
, pkgs
, ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.browsers;
in
{
  options.modules.desktop.browsers = {
    default = mkOption {
      type = with types; nullOr str;
      default = null;
      description = "Default system browser";
      example = "brave";
    };
  };

  config = mkIf (cfg.default != null) {
    env.BROWSER = cfg.default;
  };
}
