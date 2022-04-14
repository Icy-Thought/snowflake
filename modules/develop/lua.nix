{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.develop;
in {
  options.modules.develop.lua = {
    enable = mkBoolOpt false;
    xdg.enable = mkBoolOpt cfg.enableXDG;
    fennel.enable = mkBoolOpt false;
  };

  config = mkMerge [
    (mkIf cfg.lua.enable {
      user.packages = with pkgs; [
        lua
        sumneko-lua-language-server
        stylua
        (mkIf cfg.lua.fennel.enable [ fennel fnlfmt ])
      ];

      home.configFile = with config.snowflake; {
        "stylua/stylua.toml".source = "${configDir}/formatters/stylua.toml";
      };
    })

    (mkIf cfg.xdg.enable {
      # TODO
    })
  ];
}
