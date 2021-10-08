{ config, lib, pkgs, ... }:

let imports = [ ./print-colors.nix ];

in { inherit imports; }
