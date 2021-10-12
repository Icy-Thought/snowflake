{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.develop.nixLang;
in {
  options.modules.develop.nixLang = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      nix-template
      nix-update
      nix-top
      nixpkgs-review
      nix-diff
    ];
  };
}
