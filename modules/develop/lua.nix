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

  config = mkIf cfg.enable (mkMerge [
    {
      user.packages = attrValues ({
        inherit (pkgs) lua lua-language-server stylua;
      } // optionalAttrs (cfg.fennel.enable) { inherit (pkgs) fennel fnlfmt; });
    }

    (mkIf config.modules.develop.xdg.enable {
      create.configFile.stylua-conf = {
        target = "stylua/stylua.toml";
        source = let tomlFormat = pkgs.formats.toml { };
        in tomlFormat.generate "stylua-conf" {
          column_width = 80;
          line_endings = "Unix";
          indent_type = "Spaces";
          indent_width = 4;
          quote_style = "AutoPreferDouble";
          call_parentheses = "Always";
        };
      };
    })
  ]);
}
