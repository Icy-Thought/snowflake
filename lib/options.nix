{ lib, ... }:

let inherit (lib.options) mkOption;
in {
  mkOpt = type: default: mkOption { inherit type default; };

  mkOpt' = type: default: description:
    mkOption { inherit type default description; };
}
