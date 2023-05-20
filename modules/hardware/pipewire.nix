{
  config,
  options,
  lib,
  pkgs,
  ...
}: let
  inherit (lib.attrsets) attrValues;
  inherit (lib.modules) mkIf;

  cfg = config.modules.hardware.pipewire;
in {
  options.modules.hardware.pipewire = let
    inherit (lib.options) mkEnableOption;
  in {enable = mkEnableOption "modern audio support";};

  config = mkIf cfg.enable {
    user.packages = attrValues {inherit (pkgs) easyeffects;};

    security.rtkit.enable = true;

    services.pipewire = {
      enable = true;
      wireplumber.enable = true;
      alsa = {
        enable = true;
        support32Bit = true;
      };
      pulse.enable = true;
      #jack.enable = true;
    };

    home.configFile = mkIf config.modules.hardware.bluetooth.enable {
      wireplumber-bluetooth = {
        target = "wireplumber/bluetooth.lua.d/51-bluez-config.lua";
        text = ''
          bluez_monitor.properties = {
              ["bluez5.enable-sbc-xq"] = true,
              ["bluez5.enable-msbc"] = true,
              ["bluez5.enable-hw-volume"] = true,
              ["bluez5.headset-roles"] = "[ hsp_hs hsp_ag hfp_hf hfp_ag ]"
          }
        '';
      };

      wireplumber-disable-suspension = {
        target = "wireplumber/main.lua.d/51-disable-suspension.lua";
        text = ''
          table.insert(alsa_monitor.rules, {
              matches = {
                  { -- Matches all sources.
                      { "node.name", "matches", "alsa_input.*" },
                  },
                  { -- Matches all sinks.
                      { "node.name", "matches", "alsa_output.*" },
                  },
              },
              apply_properties = { ["session.suspend-timeout-seconds"] = 0 },
          })
        '';
      };
    };
  };
}
