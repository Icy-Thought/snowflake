{ config, options, lib, pkgs, ... }:
let
  inherit (lib.modules) mkIf;
  # inherit (lib.strings) concatStringsSep;
in {
  options.modules.networking.mullvad = let inherit (lib.options) mkEnableOption;
  in { enable = mkEnableOption "enable Mullvad VPN client"; };

  config = mkIf config.modules.networking.mullvad.enable {
    networking = {
      # https://github.com/NixOS/nixpkgs/issues/113589
      firewall.checkReversePath = "loose";
      wireguard.enable = true;

      # mullvad-daemon -> :w /etc/iproute2/rt_tables
      iproute2.enable = true;
    };

    services.mullvad-vpn = {
      enable = true;
      package = pkgs.mullvad-vpn;
    };
  };
}
