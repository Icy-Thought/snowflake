{ options, config, lib, pkgs, ... }:

let
  inherit (lib.attrsets) attrValues;
  inherit (lib.modules) mkIf mkMerge;
in {
  options.modules.develop.scientific = let inherit (lib.options) mkEnableOption;
  in {
    latex.enable = mkEnableOption "bloated doc/math lang";
    typst.enable = mkEnableOption "modern LaTeX alt.";
  };

  config = mkMerge [
    (mkIf config.modules.develop.scientific.latex.enable {
      user.packages = attrValues {
        inherit (pkgs) texlab;
        tex = pkgs.texlive.combine {
          # :FIXME| completely replace with typst
          inherit (pkgs.texlive)
            scheme-basic capt-of dvipng dvisvgm fancyvrb fontspec hyperref
            latexmk ulem koma-script greek-inputenc trimspaces
            # :NOTE| Mathematics-related

            amsmath cancel mathtools
            # :NOTE| Graphics-related

            parskip pgf pgfplots svg transparent wrapfig xcolor;
        };
      };

      hm.programs.vscode.extensions = attrValues {
        inherit (pkgs.vscode-extensions.james-yu) latex-workshop;
      };
    })

    (mkIf config.modules.develop.scientific.typst.enable {
      user.packages = attrValues { inherit (pkgs) typst tinymist typstyle; };

      hm.programs.vscode.extensions = attrValues {
        inherit (pkgs.vscode-extensions.myriad-dreamin) tinymist;
      };
    })

    (mkIf config.modules.develop.xdg.enable {
      # TODO:
    })
  ];
}
