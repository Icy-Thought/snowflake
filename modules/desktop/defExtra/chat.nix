{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.defExtra.chat;
in {
  options.modules.desktop.defExtra.chat = {
    enable = mkBoolOpt false;
    essentials = mkBoolOpt false;
    signal = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      (mkIf cfg.essentials.enable master.discord element-desktop tdesktop)
      (mkIf cfg.signal.enable signal-desktop)
    ];

    #TODO: discord (powercord) + declerative setup.
  };
}
