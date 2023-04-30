{ config, options, lib, pkgs, ... }:

let inherit (lib.modules) mkIf;
in {
  options.modules.services.fail2ban = let inherit (lib.options) mkEnableOption;
  in { enable = mkEnableOption false; };

  config = mkIf config.modules.services.fail2ban.enable {
    services.fail2ban = {
      enable = true;
      ignoreIP = [ "127.0.0.1/16" "192.168.1.0/24" ];
      banaction-allports = "iptables-allports";

      bantime-increment = {
        enable = true;
        maxtime = "168h";
        factor = "4";
      };

      jails.DEFAULT = ''
        blocktype = DROP
        bantime = 1h
        findtime = 1h
      '';
    };

    # Extra filters
    environment.etc = {
      vaultwarden = {
        target = "fail2ban/filter.d/vaultwarden.conf";
        text = ''
          [INCLUDES]
          before = common.conf

          [Definition]
          failregex = ^.*Username or password is incorrect\. Try again\. IP: <ADDR>\. Username:.*$
          ignoreregex =
          journalmatch = _SYSTEMD_UNIT=vaultwarden.service
        '';
      };

      gitea = {
        target = "fail2ban/filter.d/gitea.conf";
        text = ''
          [Definition]
          failregex =  .*(Failed authentication attempt|invalid credentials|Attempted access of unknown user).* from <HOST>
          ignoreregex =
          journalmatch = _SYSTEMD_UNIT=gitea.service
        '';
      };
    };
  };
}
