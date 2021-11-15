{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.appliances.extras.chat;
in {
  options.modules.appliances.extras.chat = {
    enable = mkBoolOpt false;
    signal.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      # TODO: discord (powercord) + declerative setup.
      unstable.discord
      element-desktop
      tdesktop

      (mkIf cfg.signal.enable signal-desktop)
    ];
  };
}
