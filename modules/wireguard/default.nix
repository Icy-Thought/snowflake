{ config, lib, pkgs, ... }:

let imports = [ ./config/Akkadian-VPN.nix ];

in { inherit imports; }
