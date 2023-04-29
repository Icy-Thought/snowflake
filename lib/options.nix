{ lib, ... }:

let
  inherit (lib) mkOption;
  inherit (lib.types) bool;
in {
  mkOpt = type: default: mkOption { inherit type default; };

  mkOpt' = type: default: description:
    mkOption { inherit type default description; };
}
