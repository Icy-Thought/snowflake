{ config, lib, pkgs, ... }:

with lib;
with lib.my; {
  hardware.opengl.enable = true;
  hardware.opengl.driSupport = true;
  hardware.opengl.driSupport32Bit = true;
}
