{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.gaming.emulators;
in {
  options.modules.desktop.gaming.emulators = {
    psx.enable = mkBoolOpt false; # Playstation
    ds.enable = mkBoolOpt false; # Nintendo DS
    gb.enable = mkBoolOpt false; # GameBoy + GameBoy Color
    gba.enable = mkBoolOpt false; # GameBoy Advance
    snes.enable = mkBoolOpt false; # Super Nintendo
  };

  config = {
    user.packages = [
      (mkIf (cfg.psx.enable) pkgs.epsxe)
      (mkIf (cfg.ds.enable) pkgs.desmume)
      (mkIf (cfg.gba.enable
        || cfg.gb.enable
        || cfg.snes.enable)
      pkgs.higan)
    ];
  };
}
