{ options, config, lib, pkgs, ... }:

let
  inherit (builtins) getEnv;
  inherit (lib.modules) mkIf mkMerge;
in {
  options.modules.networking.samba = let inherit (lib.options) mkEnableOption;
  in {
    sharing.enable =
      mkEnableOption "Samba: enable NixOS -> external file-transfer";
    receiving.enable =
      mkEnableOption "Samba: enable external -> NixOS file-transfer";
  };

  config = mkMerge [
    (mkIf config.modules.networking.samba.sharing.enable {
      users = {
        groups.samba-guest = { };
        users.samba-guest = {
          isSystemUser = true;
          description = "Residence of our Samba guest users";
          group = "samba-guest";
          home = "/var/empty";
          createHome = false;
          shell = pkgs.shadow;
        };
      };
      user.extraGroups = [ "samba-guest" ];

      networking.firewall = {
        allowPing = true;
        allowedTCPPorts = [ 5357 ]; # wsdd
        allowedUDPPorts = [ 3702 ]; # wsdd
      };

      services.samba-wsdd.enable =
        true; # make shares visible for windows 10 clients

      services.samba = {
        enable = true;
        openFirewall = true;
        extraConfig = ''
          server string = ${config.networking.hostName}
          netbios name = ${config.networking.hostName}
          workgroup = WORKGROUP
          security = user

          create mask = 0664
          force create mode = 0664
          directory mask = 0775
          force directory mode = 0775
          follow symlinks = yes

          # :NOTE| localhost is the ipv6 localhost ::1
          hosts allow = 192.168.0.0/16 localhost
          hosts deny = 0.0.0.0/0
          guest account = nobody
          map to guest = bad user
        '';
        # :NOTE| set sudo smbpasswd -a samba-guest -n
        shares = {
          Public = {
            path = (getEnv "HOME") + "/Public";
            browseable = "yes";
            "read only" = "yes";
            "guest ok" = "yes";
            "force user" = "${config.user.name}";
            "force group" = "samba-guest";
            "write list" = "${config.user.name}";
          };
        };
      };
    })
  ];
}
