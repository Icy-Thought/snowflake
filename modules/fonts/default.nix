{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.fonts;
in {
  options.modules.fonts = {
    settings = {
      family = mkOption {
        type = types.str;
        example = "Iosevka Nerd Font";
        description = "System-wide font to be applied";
      };

      monospace = mkOption {
        type = types.str;
        example = "Iosevka Nerd Font Mono";
        description = "System-wide mono font to be applied";
      };

      style = mkOption {
        type = types.str;
        example = "SemiBold";
        description = "Default style of defined font";
      };

      size = mkOption {
        type = types.int;
        example = "13";
        description = "Default size of defined font";
      };
    };
  };
}
