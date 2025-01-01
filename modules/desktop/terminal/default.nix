{ options, config, lib, pkgs, ... }:

let cfg = config.modules.desktop.terminal;
in with lib; {
  options.modules.desktop.terminal = with types; {
    default = mkOption {
      type = str;
      default = "xterm";
      description = "Default terminal";
      example = "kitty";
    };
  };

  config = mkMerge [
    {
      home.sessionVariables.TERMINAL = cfg.default;
      services.xserver.desktopManager.xterm.enable =
        mkDefault (cfg.default == "xterm");
    }

    (mkIf (config.modules.desktop.type == "x11") {
      services.xserver.excludePackages =
        mkIf (cfg.default != "xterm") [ pkgs.xterm ];
    })
  ];
}
