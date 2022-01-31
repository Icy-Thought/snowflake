{ inputs, config, lib, pkgs, ... }:

with lib;
with lib.my; {
  imports = [ inputs.home-manager.nixosModules.home-manager ]
    ++ (mapModulesRec' (toString ./modules) import);

  # Common config for all nixos machines;
  environment.variables = {
    SNOWFLAKE = config.snowflake.dir;
    SNOWFLAKE_BIN = config.snowflake.binDir;

    # Configure nix and nixpkgs
    NIXPKGS_ALLOW_UNFREE = "1";
  };

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

    registry = registryInputs // { snowflake.flake = inputs.self; };

    settings = {
      auto-optimise-store = true;
      substituters = [ "https://nix-community.cachix.org" ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
    };
  };

  system = {
    stateVersion = "21.11";
    configurationRevision = with inputs; mkIf (self ? rev) self.rev;
  };

  # Some reasonable, global defaults
  ## This is here to appease 'nix flake check' for generic hosts with no
  ## hardware-configuration.nix or fileSystem config.
  fileSystems."/".device = mkDefault "/dev/disk/by-label/nixos";

  boot = {
    kernelPackages = mkDefault pkgs.linuxPackages_latest;
    kernelParams = [ "pcie_aspm.policy=performance" ];

    loader = {
      efi.efiSysMountPoint = "/boot";
      efi.canTouchEfiVariables = mkDefault true;

      grub = {
        enable = mkDefault true;
        version = 2;
        device = "nodev";
        efiSupport = mkDefault true;
        useOSProber = mkDefault true;
      };
    };
  };

  console = {
    font = mkDefault "Lat2-Terminus16";
    useXkbConfig = mkDefault true;
  };

  time.timeZone = mkDefault "Europe/Berlin";
  i18n.defaultLocale = mkDefault "en_US.UTF-8";

  environment.systemPackages = with pkgs; [
    bind
    cached-nix-shell
    git
    wget
    gcc
    gnumake
    unzip
    unrar
  ];
}
