{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.inputMF.spellCheck;
in {
  options.modules.desktop.inputMF.spellCheck = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      aspell
      aspellDicts.en
      aspellDicts.sv
      hunspellDicts.sv_SE
      hunspellDicts.en_US
    ];
  };
}
