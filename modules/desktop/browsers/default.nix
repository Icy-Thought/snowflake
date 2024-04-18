{
  options,
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib.modules) mkIf;
  cfg = config.modules.desktop.browsers;
in {
  options.modules.desktop.browsers = let
    inherit (lib.options) mkOption;
    inherit (lib.types) nullOr str;
  in {
    default = mkOption {
      type = nullOr str;
      default = null;
      description = "Default system browser";
      example = "firefox";
    };
  };

  config = mkIf (cfg.default != null) {env.BROWSER = cfg.default;};
}
