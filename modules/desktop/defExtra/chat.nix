{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.defExtra.chat;
in {
  options.modules.desktop.defExtra.chat = {
    enable = mkBoolOpt false;
    discord = mkBoolOpt true;
    element = mkBoolOpt true;
    telegram = mkBoolOpt true;
    signal = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      (mkIf cfg.discord.enable master.discord)
      (mkIf cfg.element.enable element-desktop)
      (mkIf cfg.telegram.enable tdesktop)
      (mkIf cfg.signal.enable signal-desktop)
    ];

    #TODO: discord (powercord) + declerative setup.
  };
}
