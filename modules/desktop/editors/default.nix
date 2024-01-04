{
  config,
  options,
  lib,
  pkgs,
  ...
}: let
  inherit (lib.attrsets) attrValues;
  inherit (lib.modules) mkIf mkMerge;
  cfg = config.modules.desktop.editors;
in {
  options.modules.desktop.editors = let
    inherit (lib.options) mkOption;
    inherit (lib.types) nullOr enum;
  in {
    default = mkOption {
      type = nullOr (enum ["helix" "nvim" "emacsclient"]);
      default = "nvim";
      description = "Default editor for text manipulation";
      example = "emacsclient";
    };
  };

  config = mkMerge [
    (mkIf (cfg.default != null) {
      env = {
        EDITOR = cfg.default;
        OPENAI_API_KEY = "$(cat /run/agenix/ClosedAI)";
        OPENWEATHERMAP_KEY = "$(cat /run/agenix/OpenWeatherMap)";
      };
    })

    (mkIf (cfg.default == "nvim" || cfg.default == "emacsclient") {
      user.packages = attrValues {
        inherit (pkgs) imagemagick editorconfig-core-c sqlite deno pandoc nuspell;
        inherit (pkgs.hunspellDicts) en_US sv_SE;
      };
    })
  ];
}
