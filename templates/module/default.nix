{ config, options, lib, pkgs, ... }:

let
  inherit (builtins) x;
  inherit (lib) mkIf;
  inherit (lib.my) mkBoolOpt;
in {
  options.modules.X.Y = { enable = mkBoolOpt false; };

  config = mkIf config.modules.X.Y.enable { };
}
