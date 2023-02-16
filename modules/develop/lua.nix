{ config, options, lib, pkgs, ... }:

let
  inherit (lib) attrValues optionalAttrs mkIf mkMerge;
  inherit (lib.my) mkBoolOpt;

  cfg = config.modules.develop.lua;
in {
  options.modules.develop.lua = {
    enable = mkBoolOpt false;
    fennel.enable = mkBoolOpt false;
  };

  config = mkMerge [
    (mkIf cfg.enable {
      user.packages = attrValues ({
        inherit (pkgs) lua lua-language-server stylua;
      } // optionalAttrs (cfg.fennel.enable) { inherit (pkgs) fennel fnlfmt; });

      home.configFile.stylua-conf = {
        target = "stylua/stylua.toml";
        text = ''
          column_width = 80
          line_endings = "Unix"
          indent_type = "Spaces"
          indent_width = 4
          quote_style = "AutoPreferDouble"
          call_parentheses = "Always"
        '';
      };
    })

    (mkIf config.modules.develop.xdg.enable { }) # TODO
  ];
}
