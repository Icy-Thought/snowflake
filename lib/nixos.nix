{ inputs, lib, pkgs, ... }:

let
  inherit (builtins) baseNameOf elem;
  inherit (lib) filterAttrs mkDefault nixosSystem removeSuffix;
  inherit (lib.my) mapModules mkHost;

in {
  mkHost = path:
    attrs@{ system ? "x86_64-linux", ... }:
    nixosSystem {
      inherit system;

      specialArgs = { inherit lib inputs system; };

      modules = [
        {
          nixpkgs.pkgs = pkgs;
          networking.hostName =
            mkDefault (removeSuffix ".nix" (baseNameOf path));
        }
        (filterAttrs (n: v: !elem n [ "system" ]) attrs)
        ../. # /default.nix
        (import path)
      ];
    };

  mapHosts = dir:
    attrs@{ system ? system, ... }:
    mapModules dir (hostPath: mkHost hostPath attrs);
}
