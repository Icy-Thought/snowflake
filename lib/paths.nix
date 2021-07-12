{ self, lib, ... }:

with builtins;
with lib; rec {
  # ...
  conDir = toString ../.;
  modulesDir = "${conDir}/modules";
  configDir = "${modulesDir}/home-manager";
  xmonadDir = "${modulesDir}/xmonad";
  homeDir = "/home/${
      let name = getEnv "USERNAME";
      in if elem name [ "" "root" ] then "sirius" else name
    }";
}
