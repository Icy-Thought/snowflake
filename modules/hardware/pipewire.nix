{ options, config, lib, pkgs, ... }:

let cfg = config.modules.hardware.pipewire;
in with lib; {
  options.modules.hardware.pipewire = {
    enable = mkEnableOption "modern audio support";
  };

  config = mkIf cfg.enable {
    user.packages = [ pkgs.easyeffects ];

    security.rtkit.enable = true;

    services.pipewire = {
      enable = true;
      alsa = {
        enable = true;
        support32Bit = true;
      };
      pulse.enable = true;
      wireplumber = {
        enable = true;
        extraConfig = {
          "monitor.bluez.properties" = {
            "bluez5.enable-sbc-xq" = true;
            "bluez5.enable-msbc" = true;
            "bluez5.enable-hw-volume" = true;
            "bluez5.roles" = [ "hsp_hs" "hsp_ag" "hfp_hf" "hfp_ag" ];
          };

          "wireplumber.profiles" = { main."monitor.libcamera" = "disabled"; };
        };
      };
    };
  };
}
