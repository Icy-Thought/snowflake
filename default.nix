{ inputs, config, lib, pkgs, ... }:

with lib;
with lib.my; {
  imports = [ inputs.home-manager.nixosModules.home-manager ]
    ++ (mapModulesRec' (toString ./modules) import);

  # Common config for all nixos machines;
  environment.variables.SNOWFLAKE = config.snowflake.dir;
  environment.variables.SNOWFLAKE_BIN = config.snowflake.binDir;

  # Configure nix and nixpkgs
  environment.variables.NIXPKGS_ALLOW_UNFREE = "1";

  nix = let
    filteredInputs = filterAttrs (n: _: n != "self") inputs;
    nixPathInputs = mapAttrsToList (n: v: "${n}=${v}") filteredInputs;
    registryInputs = mapAttrs (_: v: { flake = v; }) filteredInputs;

  in {
    package = pkgs.nixFlakes;
    extraOptions = "experimental-features = nix-command flakes";

    nixPath = nixPathInputs ++ [
      "nixpkgs-overlays=${config.snowflake.dir}/overlays"
      "snowflake=${config.snowflake.dir}"
    ];

    binaryCaches = [ "https://nix-community.cachix.org" ];
    binaryCachePublicKeys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];

    registry = registryInputs // { snowflake.flake = inputs.self; };
    autoOptimiseStore = true;

    gc.automatic = true;
    gc.options = "--delete-older-than 3d";
  };

  system.configurationRevision = with inputs; mkIf (self ? rev) self.rev;
  system.stateVersion = "21.05";

  ## Some reasonable, global defaults
  # This is here to appease 'nix flake check' for generic hosts with no
  # hardware-configuration.nix or fileSystem config.
  fileSystems."/".device = mkDefault "/dev/disk/by-label/nixos";

  # Use the latest kernel
  boot = {
    kernelPackages = pkgs.linuxPackages_zen;
    kernelParams = [ "pcie_aspm.policy=performance" ];

    loader = {
      efi.canTouchEfiVariables = mkDefault true;
      efi.efiSysMountPoint = "/boot";

      # TODO: replace with systemd-boot when theming == possible.
      grub.enable = mkDefault true;
      grub.version = 2;
      grub.device = "nodev";
      grub.efiSupport = mkDefault true;
      grub.useOSProber = mkDefault true;
    };
  };

  environment.systemPackages = with pkgs; [
    bind
    cached-nix-shell
    git
    neovim
    wget
    gnumake
    unzip
    unrar
  ];
}
