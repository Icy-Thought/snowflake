{ config, lib, pkgs, ... }:

with lib;
with lib.my; {
  hardware.bluetooth.enable = true;
}
