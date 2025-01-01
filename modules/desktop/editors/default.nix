{ options, config, lib, pkgs, ... }:

let cfg = config.modules.desktop.editors;
in with lib; {
  options.modules.desktop.editors = with types; {
    default = mkOption {
      type = nullOr (enum [ "helix" "nvim" "emacsclient" ]);
      default = "nvim";
      description = "Default editor for text manipulation";
      example = "emacsclient";
    };
  };

  config = mkMerge [
    (mkIf (cfg.default != null) {
      home.sessionVariables = {
        EDITOR = cfg.default;
        OPENAI_API_KEY = "$(cat /run/agenix/ClosedAI)";
        OPENWEATHERMAP_KEY = "$(cat /run/agenix/OpenWeatherMap)";
      };
    })

    (mkIf (cfg.default == "nvim" || cfg.default == "emacsclient") {
      user.packages = with pkgs; [
        imagemagick
        editorconfig-core-c
        sqlite
        deno
        pandoc
        nuspell
        hunspellDicts.en_US
        hunspellDicts.sv_SE
      ];
    })
  ];
}
