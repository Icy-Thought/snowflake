{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.develop;
in {
  options.modules.develop.lua = {
    enable = mkBoolOpt false;
    fennel.enable = mkBoolOpt false;
    xdg.enable = mkBoolOpt cfg.enableXDG;
  };

  config = mkMerge [
    {
      user.packages = mkMerge (with pkgs; [
        (mkIf cfg.lua.enable [ lua sumneko-lua-language-server stylua ])
        (mkIf cfg.lua.fennel.enable [ fennel fnlfmt ])
      ]);
    }
    (mkIf cfg.lua.enable {
      home.configFile = with config.snowflake; {
        "stylua/stylua.toml".source = "${configDir}/formatters/stylua.toml";
      };
    })

    (mkIf cfg.xdg.enable {
      # TODO
    })
  ];
}
