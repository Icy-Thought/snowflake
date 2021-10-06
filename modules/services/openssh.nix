{ config, lib, pkgs, ... }:

with lib;
with lib.my; {
  # askPass -> terminal (default).
  programs.ssh.askPassword = "";

  services.openssh.enable = true;
  services.openssh.openFirewall = lib.mkDefault false;
}
