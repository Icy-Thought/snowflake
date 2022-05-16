{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.hardware.audio;
in {
  options.modules.hardware.audio = {
    enable = mkBoolOpt false;

    lowLatency = {
      enable = mkBoolOpt false;
      quantum = mkOption {
        description = "Minimum quantum to set";
        type = types.int;
        default = 64;
        example = 32;
      };
      rate = mkOption {
        description = "Rate to set";
        type = types.int;
        default = 48000;
        example = 96000;
      };
    };
  };

  config = let
    qr = "${toString cfg.lowLatency.quantum}/${toString cfg.lowLatency.rate}";
  in
    mkIf cfg.enable (mkMerge [
      {
        environment.systemPackages = with pkgs; [easyeffects];

        security.rtkit.enable = true;

        services.pipewire = {
          enable = true;
          alsa.enable = true;
          alsa.support32Bit = true;
          pulse.enable = true;
        };
      }

      (mkIf cfg.lowLatency.enable {
        services.pipewire = {
          config = {
            pipewire = {
              "context.properties" = {
                "default.clock.min-quantum" = cfg.lowLatency.quantum;
              };
            };
            pipewire-pulse = {
              "context.properties" = {};
              "context.modules" = [
                {
                  name = "libpipewire-module-rtkit";
                  args = {
                    "nice.level" = -15;
                    "rt.prio" = 88;
                    "rt.time.soft" = 200000;
                    "rt.time.hard" = 200000;
                  };
                  flags = ["ifexists" "nofail"];
                }
                {name = "libpipewire-module-protocol-native";}
                {name = "libpipewire-module-client-node";}
                {name = "libpipewire-module-adapter";}
                {name = "libpipewire-module-metadata";}
                {
                  name = "libpipewire-module-protocol-pulse";
                  args = {
                    "pulse.min.req" = qr;
                    "pulse.min.quantum" = qr;
                    "pulse.min.frag" = qr;
                    "server.address" = ["unix:native"];
                  };
                }
              ];
              "stream.properties" = {
                "node.latency" = qr;
                "resample.quality" = 1;
              };
            };
          };
          media-session.config.alsa-monitor = {
            rules = [
              {
                matches = [{node.name = "alsa_output.*";}];
                actions = {
                  update-props = {
                    "audio.format" = "S32LE";
                    "audio.rate" = cfg.lowLatency.rate * 2;
                    "api.alsa.period-size" = 2;
                  };
                };
              }
            ];
          };
        };
      })
    ]);
}
