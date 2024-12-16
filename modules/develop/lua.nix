{ options, config, lib, pkgs, ... }:

let
  inherit (lib.attrsets) attrValues optionalAttrs;
  inherit (lib.modules) mkIf mkMerge;
  cfg = config.modules.develop.lua;
in {
  options.modules.develop.lua = let inherit (lib.options) mkEnableOption;
  in {
    enable = mkEnableOption "Lua development";
    fennel.enable = mkEnableOption "Lisp-based Lua development";
  };

  config = mkIf cfg.enable (mkMerge [{
    user.packages = attrValues ({
      inherit (pkgs) lua lua-language-server stylua;
    } // optionalAttrs (cfg.fennel.enable) { inherit (pkgs) fennel fnlfmt; });
  }]);
}
