{ options, config, lib, pkgs, ... }:

with lib; {
  options.modules.virtualisation.podman = {
    enable = mkEnableOption "Enable the Podman container engine";
  };

  config = mkIf config.modules.virtualisation.podman.enable {
    virtualisation.podman = {
      enable = true;
      dockerCompat = true; # docker = podman (alias)
      # Required for containers under podman-compose to be able to talk to each other.
      defaultNetwork.dnsname.enable = true;
      # For Nixos version > 22.11
      defaultNetwork.settings = { dns_enabled = true; };
      extraPackages = with pkgs; [ conmon runc skopeo ];
    };
  };
}
