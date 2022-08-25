{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.develop.node;
  devCfg = config.modules.develop.xdg;
in {
  options.modules.develop.node = {
    enable = mkBoolOpt false;
  };

  config = mkMerge [
    (mkIf cfg.enable {
      user.packages = with pkgs; [nodejs_latest yarn];

      # Run locally installed bin-script, e.g. n coffee file.coffee
      environment.shellAliases = {
        n = ''PATH="$(${pkgs.nodejs_latest}/bin/npm bin):$PATH"'';
        ya = "yarn";
      };

      env.PATH = ["$(${getExe pkgs.yarn} global bin)"];
    })

    (mkIf devCfg.enable {
      env = {
        NPM_CONFIG_USERCONFIG = "$XDG_CONFIG_HOME/npm/config";
        NPM_CONFIG_CACHE = "$XDG_CACHE_HOME/npm";
        NPM_CONFIG_TMP = "$XDG_RUNTIME_DIR/npm";
        NPM_CONFIG_PREFIX = "$XDG_CACHE_HOME/npm";
        NODE_REPL_HISTORY = "$XDG_CACHE_HOME/node/repl_history";
      };

      home.configFile."npm/config".text = ''
        cache=$XDG_CACHE_HOME/npm
        prefix=$XDG_DATA_HOME/npm
      '';
    })
  ];
}
