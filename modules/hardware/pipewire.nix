{ config, options, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkMerge mkOption;
  inherit (lib.types) int;
  inherit (lib.my) mkBoolOpt;

  cfg = config.modules.hardware.pipewire;
in {
  options.modules.hardware.pipewire = {
    enable = mkBoolOpt false;
    lowLatency = { enable = mkBoolOpt false; };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      user.packages = [ pkgs.easyeffects ];

      security.rtkit.enable = true;

      services.pipewire = {
        enable = true;
        alsa.enable = true;
        alsa.support32Bit = true;
        pulse.enable = true;
        #jack.enable = true;
      };
    })

    (mkIf (cfg.enable && cfg.lowLatency.enable) {
      services.pipewire = {
        config.pipewire = {
          "context.properties" = {
            "link.max-buffers" = 16;
            "log.level" = 2;
            "default.clock.rate" = 48000;
            "default.clock.quantum" = 32;
            "default.clock.min-quantum" = 32;
            "default.clock.max-quantum" = 32;
            "core.daemon" = true;
            "core.name" = "pipewire-0";
          };
          "context.modules" = [
            {
              name = "libpipewire-module-rtkit";
              args = {
                "nice.level" = -15;
                "rt.prio" = 88;
                "rt.time.soft" = 200000;
                "rt.time.hard" = 200000;
              };
              flags = [ "ifexists" "nofail" ];
            }
            { name = "libpipewire-module-protocol-native"; }
            { name = "libpipewire-module-profiler"; }
            { name = "libpipewire-module-metadata"; }
            { name = "libpipewire-module-spa-device-factory"; }
            { name = "libpipewire-module-spa-node-factory"; }
            { name = "libpipewire-module-client-node"; }
            { name = "libpipewire-module-client-device"; }
            {
              name = "libpipewire-module-portal";
              flags = [ "ifexists" "nofail" ];
            }
            {
              name = "libpipewire-module-access";
              args = { };
            }
            { name = "libpipewire-module-adapter"; }
            { name = "libpipewire-module-link-factory"; }
            { name = "libpipewire-module-session-manager"; }
          ];
        };
      };
    })
  ];
}
