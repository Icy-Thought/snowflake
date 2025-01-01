{ options, config, lib, pkgs, ... }:

with lib; {
  options.modules.X.Y = { enable = mkEnableOption "Y option for X module"; };

  config = mkIf config.modules.X.Y.enable { };
}
