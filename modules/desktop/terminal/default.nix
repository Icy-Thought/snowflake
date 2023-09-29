{
  options,
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib.modules) mkDefault mkIf mkMerge;
  cfg = config.modules.desktop.terminal;
in {
  options.modules.desktop.terminal = let
    inherit (lib.options) mkOption;
    inherit (lib.types) str;
  in {
    default = mkOption {
      type = str;
      default = "xterm";
      description = "Default terminal";
      example = "kitty";
    };
  };

  config = mkMerge [
    {
      services.xserver.desktopManager.xterm.enable =
        mkDefault (cfg.default == "xterm");
      env.TERMINAL = cfg.default;
    }

    (mkIf (config.modules.desktop.envProto == "x11") {
      services.xserver.excludePackages =
        mkIf (cfg.default != "xterm") [pkgs.xterm];
    })
  ];
}
