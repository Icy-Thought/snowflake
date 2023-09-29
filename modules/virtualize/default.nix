{
  options,
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib.modules) mkIf;
  cfg = config.modules.virtualize;
in {
  options.modules.virtualize = let
    inherit (lib.options) mkEnableOption;
  in {
    enable = mkEnableOption "Spawn virtual envionrments where required.";
  };

  config = mkIf cfg.enable {
    user.packages = pkgs.virt-manager;

    virtualisation.libvirtd = {
      enable = true;
      extraOptions = ["--verbose"];
    };
    user.extraGroups = ["libvirtd"];
  };
}
