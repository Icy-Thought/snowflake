{ config, options, lib, pkgs, ... }:
let
  inherit (builtins) x;
  inherit (lib.modules) mkIf;
in {
  options.modules.X.Y = let inherit (lib.options) mkEnableOption;
  in { enable = mkEnableOption "Y option for X module"; };

  config = mkIf config.modules.X.Y.enable { };
}
