{ options, config, lib, pkgs, ... }:

let cfg = config.modules.develop.lua;
in with lib; {
  options.modules.develop.lua = {
    enable = mkEnableOption "Lua development";
    fennel.enable = mkEnableOption "Lisp-based Lua development";
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs;
      [ lua lua-language-server stylua ]
      ++ optionals (cfg.fennel.enable) [ fennel fnlfmt ];
  };
}
