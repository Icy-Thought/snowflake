{ inputs, config, options, lib, pkgs, ... }:
let
  inherit (builtins) pathExists readFile;
  inherit (lib.modules) mkIf;

  cfg = config.modules.hardware.kmonad;
in {
  options.modules.hardware.kmonad = let
    inherit (lib.options) mkEnableOption mkOption;
    inherit (lib.types) nullOr path;
  in {
    enable = mkEnableOption "advanced kbd management";
    deviceID = mkOption {
      type = nullOr path;
      default = null;
      example = "/dev/input/by-path/platform-i8042-serio-0-event-kbd";
      description = "path to device.kbd file";
    };
  };

  imports = let inherit (inputs) kmonad; in [ kmonad.nixosModules.default ];

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
          config = if pathExists layoutFile then readFile layoutFile else "";
        };
      };
  };
}
