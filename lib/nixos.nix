{ inputs, lib, pkgs, self, ... }:

with lib; rec {
  mkHost = path:
    attrs@{ system ? "x86_64-linux", ... }:
    lib.nixosSystem {
      inherit system;
      specialArgs = { inherit lib inputs system; };
      modules = [
        {
          nixpkgs.pkgs = pkgs;
          networking.hostName =
            mkDefault (removeSuffix ".nix" (builtins.baseNameOf path));
        }
        (filterAttrs (n: v: !builtins.elem n [ "system" ]) attrs)
        ../. # /default.nix
        (import path)
      ];
    };

  mapHosts = dir:
    attrs@{ system ? system, ... }:
    self.mapModules dir (hostPath: mkHost hostPath attrs);
}
