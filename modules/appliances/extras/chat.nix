{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.appliances.extras.chat;
in {
  options.modules.appliances.extras.chat = {
    enable = mkBoolOpt false;
    essential.enable = mkBoolOpt true;
    mobile.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs;
      (if cfg.essential.enable then [
        element-desktop
        unstable.discord
      ] else
        [ ]) ++

      (if cfg.mobile.enable then [
        signal-desktop
        tdesktop
        whatsapp-for-linux
      ] else
        [ ]);

    # TODO: discord (powercord) + declerative setup.
    # home.configFile = mkIf cfg.essential.enable {
    #   Powercord-related settings..
    # };
  };
}
