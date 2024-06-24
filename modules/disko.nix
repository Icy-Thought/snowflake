{
  options,
  config,
  inputs,
  lib,
  pkgs,
  ...
}: let
  inherit (builtins) pathExists;
  inherit (lib) optional;
  inherit (inputs) disko;

  diskoConf = "${config.snowflake.hostDir}/partition.nix";
in {
  imports =
    [disko.nixosModules.default]
    ++ optional (pathExists diskoConf) (import diskoConf {});
}
