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
    {
      user.packages = mkMerge (with pkgs; [
        (mkIf cfg.enable [lua sumneko-lua-language-server stylua])
        (mkIf cfg.fennel.enable [fennel fnlfmt])
      ]);
    }
    (mkIf cfg.enable {
      home.configFile = with config.snowflake; {
        "stylua/stylua.toml".source = "${configDir}/formatters/stylua.toml";
      };
    })

    (mkIf devCfg.enable {
      # TODO
    })
  ];
}
