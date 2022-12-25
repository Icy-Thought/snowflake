{ inputs
, config
, options
, lib
, pkgs
, ...
}:
with lib;
with lib.my;

let cfg = config.modules.hardware.kmonad;
in
{
  options.modules.hardware.kmonad = {
    enable = mkBoolOpt false;
    deviceID = mkOption {
      type = with types; nullOr path;
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
      let layoutFile = "${builtins.toString ../hosts}/${config.networking.hostName}/kmonad/layout.kbd";
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
            then [ layoutFile ]
            else { };
        };
      };
  };
}
