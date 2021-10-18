{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.defExtra.chat;
in {
  options.modules.desktop.defExtra.chat = {
    enable = mkBoolOpt false;
    discord.enable = mkBoolOpt true;
    element.enable = mkBoolOpt true;
    telegram.enable = mkBoolOpt true;
    signal.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs;
      (if cfg.discord.enable then [ master.discord ] else [ ])

      ++ (if cfg.element.enable then [ element-desktop ] else [ ])

      ++ (if cfg.telegram.enable then [ tdesktop ] else [ ])

      ++ (if cfg.signal.enable then [ signal-desktop ] else [ ]);

    # TODO: discord (powercord) + declerative setup.

  };
}
