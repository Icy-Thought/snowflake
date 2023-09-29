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
    inherit (lib.types) str;
  in {
    default = mkOption {
      type = str;
      default = "nvim";
      description = "Default editor for text manipulation";
      example = "emacs";
    };
  };

  config = mkMerge [
    (mkIf (cfg.default != null) {
      env = {
        EDITOR = cfg.default;
        OPENAI_API_KEY = "$(cat /run/agenix/closedAI)";
      };
    })

    (mkIf (cfg.default == "nvim" || cfg.default == "emacs") {
      user.packages = attrValues {
        inherit (pkgs) imagemagick editorconfig-core-c sqlite deno pandoc nuspell;
        inherit (pkgs.hunspellDicts) en_US sv_SE;
      };
    })
  ];
}
