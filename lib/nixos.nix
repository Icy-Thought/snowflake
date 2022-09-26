{ inputs
, lib
, pkgs
, ...
}:
with lib;
with lib.my; {
  mkHost = path: attrs @ { system ? "x86_64-linux", ... }:
    nixosSystem {
      inherit system;

      specialArgs = { inherit lib inputs system; };

      modules = [{
        nixpkgs.pkgs = pkgs;
        networking.hostName =
          mkDefault (removeSuffix ".nix" (baseNameOf path));
      }
        (filterAttrs (n: v: !elem n [ "system" ]) attrs)
        ../. # /default.nix
        (import path)];
    };

  mapHosts = dir: attrs @ { system ? system, ... }:
    mapModules dir (hostPath: mkHost hostPath attrs);
}
