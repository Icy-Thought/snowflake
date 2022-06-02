# Guide Towards a Functional `wg-quick`!

# Table of Contents
- [Introduction](#introduction)
- [(Template) Example Wireguard Connection](#template-example-wireguard-connection)
- [Example Configuration](#example-configuration)

# Introduction
Setting up a functional `wg-quick` environment in NixOS can be bothersome oftentimes to due non-functional operations that would otherwise work in a normal linux environment. 
An example of such scenario is the usage of `%i` in both the `postUp` & `preDown`.

Henceforth I have choosen to write both a template and an example showcasing how a nixified wg-quick configuration could look like.

# (Template) Example Wireguard Connection
```nix
{ config, lib, pkgs, ... }: {

  # Disable wg-quick on boot:
  systemd.services."wg-quick-wg0" = {
    requires = [ "network-online.target" ];
    after = [ "network.target" "network-online.target" ];
    wantedBy = lib.mkForce [ ];
    environment.DEVICE = "wg0";
  };

  networking.wg-quick.interfaces = {
    wg0 = {
      address = [ "" ];
      dns = [ "" ];

      listenPort = 51820;
      privateKeyFile = "/run/secrets/wg0/private-key"; # Agenix

      peers = [{
        publicKey = "";
        allowedIPs = [ "" ];
        endpoint = "";
        persistentKeepalive = 25;
      }];

      postUp = ''
        iptables -I OUTPUT ! -o wg0 \
          -m mark ! --mark $(wg show wg0 fwmark) \
          -m addrtype ! --dst-type LOCAL -j REJECT

        ip6tables -I OUTPUT ! -o wg0 \
          -m mark ! --mark $(wg show wg0 fwmark) \
          -m addrtype ! --dst-type LOCAL -j REJECT
      '';

      preDown = ''
        iptables -D OUTPUT ! -o wg0 \
          -m mark ! --mark $(wg show wg0 fwmark) \
          -m addrtype ! --dst-type LOCAL -j REJECT

        ip6tables -D OUTPUT ! -o wg0 \
          -m mark ! --mark $(wg show wg0 fwmark) \
          -m addrtype ! --dst-type LOCAL -j REJECT
      '';
    };
  };
}
```

# Example Configuration
> **HEADS UP!** 
> The information displayed in the following example originates from the man-page of `wg-quick` and also Quad9 DNS servers, which means that the information has been written with an educational purpose in mind.
> Therefore I do not claim the ownership of those settings nor am I trying to leak information belonging to a netizen.

```nix
{ config, lib, pkgs, ... }: {

  # Disable wg-quick on boot:
  systemd.services."wg-quick-ghostVPN" = {
    requires = [ "network-online.target" ];
    after = [ "network.target" "network-online.target" ];
    wantedBy = lib.mkForce [ ];
    environment.DEVICE = "ghostVPN";
  };

  networking.wg-quick.interfaces = {
    ghostVPN = {
      address = [ "10.192.122.1/24" "10.10.0.1/16" ];
      dns = [
        "9.9.9.9"
        "149.112.112.112"
        "2620:fe::fe"
        "2620:fe::9"
      ];

      listenPort = 51820;
      privateKeyFile = "/run/secrets/ghostVPN/private-key"; # Agenix

      peers = [{
        publicKey = "xTIBA5rboUvnH4htodjb6e697QjLERt1NAB4mZqp8Dg=";
        allowedIPs = [ "10.192.122.3/32" "10.192.124.1/24" ];
        endpoint = "209.202.254.14:8172";
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
```
