{ config, lib, pkgs, ... }:

with lib;
with lib.my; {
  # Reduce boot-time on ThinkPad:
  systemd.services.systemd-resolved.enable = true;
  systemd.services.systemd-machined.enable = false;

  # Battery control + Printing through CUPS:
  services.upower.enable = true;
  services.printing.enable = true;
}
