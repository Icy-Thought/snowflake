# Guide Towards a Functional `wg-quick`!

# Table of Contents
- [Introduction](#introduction)
- [(Template) Example Wireguard Connection](#template-example-wireguard-connection)
- [Example Configuration](#example-configuration)

# Introduction

Setting up a functional `wg-quick` environment in NixOS can be bothersome oftentimes to due non-functional operations that would otherwise work in a normal linux environment.
An example of such scenario is the usage of `%i` in both the `postUp` & `preDown`.

Henceforth I have choosen to write both a template and an example showcasing how a nixified wg-quick configuration could look like.

> [!WARNING]
> Verify the status of the kill-switch (`.nix` || `.conf`) by manually deleting the addr and connecting to the internet.
> If a connection has been established, leak is present. Otherwise we are good to go!
> CMD: `sudo ip a del <IP_Address> dev <Interface-Name>` (`del` -> `add` to restore connection)

# Conf-based WireGuard Template

## Kill-Switch: Terminate connection on leak!

Expand upon the pre-existing `[Interface]` section of your `insert-vpn.conf` file with the following lines:

```conf
PostUp  =  iptables -I OUTPUT ! -o %i -m mark ! --mark $(wg show %i fwmark) -m addrtype ! --dst-type LOCAL -j REJECT && ip6tables -I OUTPUT ! -o %i -m mark ! --mark $(wg show %i fwmark) -m addrtype ! --dst-type LOCAL -j REJECT

PreDown = iptables -D OUTPUT ! -o %i -m mark ! --mark $(wg show  %i fwmark) -m addrtype ! --dst-type LOCAL -j REJECT && ip6tables -D OUTPUT ! -o %i -m mark ! --mark $(wg show  %i fwmark) -m addrtype ! --dst-type LOCAL -j REJECT
```

# Nix-based WireGuard Template

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
