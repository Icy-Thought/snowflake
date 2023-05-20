{
  config,
  options,
  lib,
  pkgs,
  ...
}: let
  inherit (lib.attrsets) attrValues;
  inherit (lib.modules) mkIf mkMerge;
  inherit (lib.meta) getExe;
in {
  options.modules.develop.node = let
    inherit (lib.options) mkEnableOption;
  in {enable = mkEnableOption "JS/TS development";};

  config = mkMerge [
    (mkIf config.modules.develop.node.enable {
      user.packages = attrValues {inherit (pkgs) nodejs_latest yarn;};

      # Run locally installed bin-script, e.g. n coffee file.coffee
      environment.shellAliases = {
        n = ''PATH="$(${pkgs.nodejs_latest}/bin/npm bin):$PATH"'';
        ya = "yarn";
      };

      env.PATH = ["$(${getExe pkgs.yarn} global bin)"];
    })

    (mkIf config.modules.develop.xdg.enable {
      env = {
        NPM_CONFIG_USERCONFIG = "$XDG_CONFIG_HOME/npm/config";
        NPM_CONFIG_CACHE = "$XDG_CACHE_HOME/npm";
        NPM_CONFIG_TMP = "$XDG_RUNTIME_DIR/npm";
        NPM_CONFIG_PREFIX = "$XDG_CACHE_HOME/npm";
        NODE_REPL_HISTORY = "$XDG_CACHE_HOME/node/repl_history";
      };

      home.configFile.npm-conf = {
        target = "npm/config";
        text = ''
          cache=$XDG_CACHE_HOME/npm
          prefix=$XDG_DATA_HOME/npm
        '';
      };
    })
  ];
}
