{ config, options, lib, pkgs, ... }:

let
  inherit (lib) attrValues mkIf mkMerge mkOption;
  inherit (lib.types) str;

  cfg = config.modules.desktop.editors;
in {
  options.modules.desktop.editors = {
    default = mkOption {
      type = str;
      default = "nvim";
      description = "Default editor for text manipulation";
      example = "emacs";
    };
  };

  config = mkMerge [
    (mkIf (cfg.default != null) { env.EDITOR = cfg.default; })

    (mkIf (cfg.default == "nvim" || cfg.default == "emacs") {
      user.packages = attrValues ({
        inherit (pkgs)
          imagemagick editorconfig-core-c sqlite deno pandoc texlab;
        aspellPlusDict = pkgs.aspellWithDicts
          (dict: with dict; [ en en-computers en-science ]);
        tex = pkgs.texlive.combine {
          inherit (pkgs.texlive)
          # General
            scheme-basic capt-of dvipng dvisvgm fancyvrb fontspec hyperref
            latexmk ulem koma-script greek-inputenc trimspaces
            # Mathematics
            amsmath cancel mathtools
            # Graphics
            parskip pgf pgfplots svg transparent wrapfig xcolor;
        };
      });
    })
  ];
}
