{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.develop.node;
  node = pkgs.nodejs_latest;
in {
  options.modules.develop.node = {
    enable = mkBoolOpt false;
    enableGlobally = mkBoolOpt false;
  };

  config = mkIf cfg.enable (mkMerge [
    (mkIf cfg.enableGlobally {
      user.packages = with pkgs; [ node yarn ];

      # Run locally installed bin-script, e.g. n coffee file.coffee
      environment.shellAliases = {
        n = ''PATH="$(${node}/bin/npm bin):$PATH"'';
        ya = "yarn";
      };

      env.PATH = [ "$(${pkgs.yarn}/bin/yarn global bin)" ];
    })

    {
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
    }
  ]);
}
