{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.terminal;
in {
  options.modules.desktop.terminal = {
    default = mkOption {
      type = with types; str;
      default = "xterm";
      description = "Default terminal";
      example = "kitty";
    };
  };

  config = {
    services.xserver.desktopManager.xterm.enable =
      mkDefault (cfg.default == "xterm");

    env.TERMINAL = cfg.default;
  };
}
