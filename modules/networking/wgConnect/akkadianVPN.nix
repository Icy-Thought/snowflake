{ options, config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkForce;
  inherit (lib.my) mkBoolOpt;
in {
  options.modules.networking.akkadianVPN = { enable = mkBoolOpt false; };

  config = mkIf config.modules.networking.akkadianVPN.enable {
    # Enabling WireGuard settings:
    modules.networking.wireGuard.enable = true;

    systemd.services."wg-quick-akkadianVPN" = {
      requires = [ "network-online.target" ];
      after = [ "network.target" "network-online.target" ];
      wantedBy = mkForce [ ];
      environment.DEVICE = "akkadianVPN";
    };

    networking.wg-quick.interfaces.akkadianVPN = {
      address = [ "10.0.111.240/24" "fdab:1337:1337:111::240/64" ];
      dns = [
        "2001:9b1:8826::53"
        "2001:9b0:4:2601::53"
        "98.128.186.86"
        "155.4.89.136"
      ];

      listenPort = 51820;

      privateKeyFile = config.age.secrets.akkadianVPN.path;

      peers = [{
        publicKey = "ipW9/ysMc9vQbg/x7WK/udnl06+NJioWZZ4XIqz4PQY=";
        allowedIPs = [ "0.0.0.0/0" "::/0" ];
        endpoint = "wireguard.5july.net:48574";
        persistentKeepalive = 25;
      }];

      postUp = ''
        iptables -I OUTPUT ! -o akkadianVPN \
          -m mark ! --mark $(wg show akkadianVPN fwmark) \
          -m addrtype ! --dst-type LOCAL -j REJECT

        ip6tables -I OUTPUT ! -o akkadianVPN \
          -m mark ! --mark $(wg show akkadianVPN fwmark) \
          -m addrtype ! --dst-type LOCAL -j REJECT
      '';

      preDown = ''
        iptables -D OUTPUT ! -o akkadianVPN \
          -m mark ! --mark $(wg show akkadianVPN fwmark) \
          -m addrtype ! --dst-type LOCAL -j REJECT

        ip6tables -D OUTPUT ! -o akkadianVPN \
          -m mark ! --mark $(wg show akkadianVPN fwmark) \
          -m addrtype ! --dst-type LOCAL -j REJECT
      '';
    };
  };
}
