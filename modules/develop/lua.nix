{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.develop.lua;
  devCfg = config.modules.develop.xdg;
in {
  options.modules.develop.lua = {
    enable = mkBoolOpt false;
    fennel.enable = mkBoolOpt false;
  };

  config = mkMerge [
    (mkIf cfg.enable {
      user.packages = with pkgs; [
        lua
        sumneko-lua-language-server
        stylua
      ];

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

    (mkIf cfg.fennel.enable {
      user.packages = with pkgs; [fennel fnlfmt];
    })

    (mkIf devCfg.enable {}) # TODO
  ];
}
