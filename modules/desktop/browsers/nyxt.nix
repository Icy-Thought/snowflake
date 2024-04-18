{
  config,
  options,
  lib,
  pkgs,
  ...
}: let
  inherit (lib.attrsets) attrValues mergeAttrsList;
  inherit (lib.modules) mkIf;
  configDir = config.snowflake.configDir;
in {
  options.modules.desktop.browsers.nyxt = let
    inherit (lib.options) mkEnableOption;
  in {enable = mkEnableOption "The hacker's browser";};

  config = mkIf config.modules.desktop.browsers.nyxt.enable {
    user.packages = attrValues {
      inherit (pkgs) nyxt;
    };

    home.configFile."nyxt" = let
      inherit (config.modules.themes) active;
      font = "${active.font.sans}";
      colors = "${active.colors}";
    in {
      source = "${configDir}/nyxt";
      recursive = true;
    };

    home.dataFile = let
      plugins = [
        "nx-dark-reader"
        "nx-search-engines"
        "nx-kaomoji"
      ];
    in
      mergeAttrsList (map
        (extension: {
          "nyxt/extensions/${extension}".source = pkgs.sources.${extension};
        })
        plugins);
  };
}
