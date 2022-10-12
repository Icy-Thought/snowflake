{ inputs
, config
, options
, lib
, pkgs
, ...
}:
with lib;
with lib.my;

let
  cfg = config.modules.desktop.extra.kmonad;
  configDir = config.snowflake.configDir;
in
{
  options.modules.desktop.extra.kmonad = {
    enable = mkBoolOpt false;
    deviceID = mkOption {
      type = with types; nullOr str;
      default = null;
      example = "/dev/input/by-path/platform-i8042-serio-0-event-kbd";
      description = "path to device.kbd file";
    };
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays = [ inputs.kmonad.overlays.default ];

    # Import KMonad Nix-Module
    hm.imports = [ inputs.kmonad.nixosModules.default ];

    hm.services.kmonad = {
      enable = true;
      keyboards.hyperCtl = {
        device = cfg.deviceID;
        defcfg = {
          enable = true;
          fallthrough = true;
          allowCommands = false;
          compose.key = null;
        };
        # TODO Fetch from hostname + fix conf
        config = builtins.readFile (configDir + "/kmonad/hyperCtl.kbd");
      };
    };
  };
}
