{ inputs, lib, pkgs, self, ... }:

let
  inherit (inputs.nixpkgs.lib) nixosSystem;
  inherit (builtins) baseNameOf elem;
  inherit (lib.attrsets) filterAttrs;
  inherit (lib.modules) mkDefault;
  inherit (lib.strings) removeSuffix;
  inherit (self.modules) mapModules;
in rec {
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
