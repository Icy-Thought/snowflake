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
{ config, lib, pkgs, ... }:

let interface = "wg0";
in with lib; {
  systemd.services."wg-quick-${interface}" = {
    requires = [ "network-online.target" ];
    after = [ "network.target" "network-online.target" ];
    wantedBy = lib.mkForce [ ];
    environment.DEVICE = "${interface}";
  };

  networking.wg-quick.interfaces.${interface} = {
    address = [ "" ];
    dns = [ "" ];

    listenPort = 51820;
    privateKeyFile = "/run/secrets/${interface}/private-key"; # Agenix

    peers = [{
      publicKey = "";
      allowedIPs = [ "" ];
      endpoint = "";
      persistentKeepalive = 25;
    }];

    postUp = ''
      iptables -I OUTPUT ! -o ${interface} \
        -m mark ! --mark $(wg show ${interface} fwmark) \
        -m addrtype ! --dst-type LOCAL -j REJECT

      ip6tables -I OUTPUT ! -o ${interface} \
        -m mark ! --mark $(wg show ${interface} fwmark) \
        -m addrtype ! --dst-type LOCAL -j REJECT
    '';

    preDown = ''
      iptables -D OUTPUT ! -o ${interface} \
        -m mark ! --mark $(wg show ${interface} fwmark) \
        -m addrtype ! --dst-type LOCAL -j REJECT

      ip6tables -D OUTPUT ! -o ${interface} \
        -m mark ! --mark $(wg show ${interface} fwmark) \
        -m addrtype ! --dst-type LOCAL -j REJECT
    '';
    };
  };
}
```
