{ options, config, lib, pkgs, ... }:

with lib; {
  options.modules.networking.wireguard.ghostVPN = {
    enable = mkEnableOption "ghostVPN conf. for wireguard";
  };

  # TODO: replace ghostVPN with desired module name!
  config = mkIf config.modules.networking.wireguard.ghostVPN.enable {
    modules.networking.wireguard.enable = true;

    systemd.services."wg-quick-ghostVPN" = {
      requires = [ "network-online.target" ];
      after = [ "network.target" "network-online.target" ];
      wantedBy = mkForce [ ];
      environment.DEVICE = "ghostVPN";
    };

    networking.wg-quick.interfaces.ghostVPN = {
      address = [ "" ];
      dns = [ ];

      listenPort = 51820;

      privateKeyFile = config.age.secrets.ghostVPN.path;

      peers = [{
        publicKey = "";
        allowedIPs = [ "0.0.0.0/0" "::/0" ];
        endpoint = "";
        persistentKeepalive = 25;
      }];

      postUp = ''
        iptables -I OUTPUT ! -o ghostVPN \
          -m mark ! --mark $(wg show ghostVPN fwmark) \
          -m addrtype ! --dst-type LOCAL -j REJECT

        ip6tables -I OUTPUT ! -o ghostVPN \
          -m mark ! --mark $(wg show ghostVPN fwmark) \
          -m addrtype ! --dst-type LOCAL -j REJECT
      '';

      preDown = ''
        iptables -D OUTPUT ! -o ghostVPN \
          -m mark ! --mark $(wg show ghostVPN fwmark) \
          -m addrtype ! --dst-type LOCAL -j REJECT

        ip6tables -D OUTPUT ! -o ghostVPN \
          -m mark ! --mark $(wg show ghostVPN fwmark) \
          -m addrtype ! --dst-type LOCAL -j REJECT
      '';
    };
  };
}
