{ config, options, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkMerge;
  inherit (lib.my) mkBoolOpt;

  cfg = config.modules.develop.lua;
in {
  options.modules.develop.lua = {
    enable = mkBoolOpt false;
    fnlized.enable = mkBoolOpt false;
  };

  config = mkMerge [
    (mkIf cfg.enable {
      user.packages = with pkgs; [ lua sumneko-lua-language-server stylua ];

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

    (mkIf cfg.fnlized.enable { user.packages = with pkgs; [ fennel fnlfmt ]; })

    (mkIf config.modules.develop.xdg.enable { }) # TODO
  ];
}
