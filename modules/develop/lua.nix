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
      user.packages = [
        pkgs.lua
        pkgs.sumneko-lua-language-server
        pkgs.stylua
      ];
      home.configFile = with config.snowflake; {
        "stylua/stylua.toml".source = "${configDir}/formatters/stylua.toml";
      };
    })

    (mkIf cfg.fennel.enable {
      user.packages = [
        pkgs.fennel
        pkgs.fnlfmt
      ];
    })

    (mkIf devCfg.enable {
      # TODO
    })
  ];
}
