{ inputs
, config
, options
, lib
, pkgs
, ...
}:

let inherit (builtins) pathExists readFile;
  inherit (lib) mkIf mkOption;
  inherit (lib.types) nullOr path;
  inherit (lib.my) mkBoolOpt;

  cfg = config.modules.hardware.kmonad;
in
{
  options.modules.hardware.kmonad = {
    enable = mkBoolOpt false;
    deviceID = mkOption {
      type = nullOr path;
      default = null;
      example = "/dev/input/by-path/platform-i8042-serio-0-event-kbd";
      description = "path to device.kbd file";
    };
  };

  imports = [ inputs.kmonad.nixosModules.default ];

  config = mkIf cfg.enable {
    # Allow our user to benefit from KMonad:
    user.extraGroups = [ "uinput" ];

    services.kmonad =
      let layoutFile = "${config.snowflake.hostDir}/kmonad/layout.kbd";
      in {
        enable = true;
        keyboards.options = {
          device = cfg.deviceID;
          defcfg = {
            enable = true;
            fallthrough = true; # when keys /= assigned -> defsrc value
            allowCommands = false;
            compose.key = null;
          };
          config =
            if pathExists layoutFile
            then readFile layoutFile
            else "";
        };
      };
  };
}
