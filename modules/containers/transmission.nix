{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.containers.transmission;
in {
  options.modules.containers.transmission = {
    enable = mkBoolOpt false;

    username = mkOption {
      type = types.str;
      default = "Alonzo";
      example = "Username";
      description = "Transmission RPC User-Name";
    };

    password = mkOption {
      type = types.str;
      default = builtins.readFile config.age.secrets.transmission.path;
      example = "Password";
      description = "Transmission RPC User-Password";
    };
  };

  config = mkIf cfg.enable {
    user = {
      packages = with pkgs; [ transmission-remote-gtk ];
      extraGroups = [ "transmission" ];
    };

    containers.transmission = {
      autoStart = false;
      ephemeral = true;
      enableTun = true;

      privateNetwork = true;
      hostAddress = "192.168.17.10";
      localAddress = "192.168.17.11";

      bindMounts = {
        "/run/agenix/akkadianVPN" = {
          hostPath = config.age.secrets.akkadianVPN.path;
          isReadOnly = true;
        };

        "/var/lib/transmission" = {
          hostPath = "/var/lib/transmission";
          isReadOnly = false;
        };
      };
    };

    containers.transmission.config = {
      systemd.services.transmission = {
        bindsTo = [ "wg-quick-akkadianVPN" ];
        after = [ "wg-quick-akkadianVPN" ];
      };

      networking.nat = {
        enable = lib.mkDefault true;
        internalInterfaces = [ "ve-transmission" ];
      };

      networking.firewall = {
        enable = true;
        allowedTCPPorts = [ 51413 ];
        allowedUDPPorts = [ 51413 ];
      };

      services.transmission = {
        enable = true;
        settings = {
          # Manage transmisson file locations:
          download-dir = "/torrents";
          incomplete-dir = "/torrents/.incomplete";
          incomplete-dir-enabled = true;
          watch-dir-enabled = false;
          rename-partial-files = true;

          # Control transmisson ports:
          rpc-port = 9091;
          rpc-whitelist-enabled = true;
          rpc-whitelist = "127.0.0.1,192.168.*.*";
          rpc-bind-address = "0.0.0.0";

          rpc-authentication-required = true;
          rpc-username = cfg.username;
          rpc-password = cfg.password;

          peer-port = 51413;
          peer-socket-tos = "lowcost";

          # Extras settings:
          umask = 2;
          encryption = 2;
          message-level = 1;
          utp-enabled = true;

          ratio-limit = 0;
          ratio-limit-enabled = true;

          speed-limit-up-enabled = true;
          speed-limit-up = 1;
          idle-seeding-limit-enabled = true;
          idle-seeding-limit = 0;
        };
      };
    };
  };
}
