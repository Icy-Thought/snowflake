{ config, options, lib, pkgs, ... }:

let
  inherit (lib) attrValues mkIf mkMerge;
  inherit (lib.my) mkBoolOpt;
in {
  options.modules.develop.scientific = {
    latex.enable = mkBoolOpt false;
    typst.enable = mkBoolOpt false;
  };

  config = mkMerge [
    (mkIf config.modules.develop.scientific.latex.enable {
      user.packages = attrValues ({
        inherit (pkgs) texlab;
        tex = pkgs.texlive.combine { # FIXME: completely replace with typst
          inherit (pkgs.texlive)
            scheme-basic capt-of dvipng dvisvgm fancyvrb fontspec hyperref
            latexmk ulem koma-script greek-inputenc trimspaces
            # Mathematics
            amsmath cancel mathtools
            # Graphics
            parskip pgf pgfplots svg transparent wrapfig xcolor;
        };
      });

      hm.programs.vscode.extensions = attrValues ({
        inherit (pkgs.vscode-extensions.james-yu) latex-workshop;
      });
    })

    (mkIf config.modules.develop.scientific.typst.enable {
      user.packages = attrValues ({ inherit (pkgs) typst typst-lsp; });

      hm.programs.vscode.extensions =
        attrValues ({ inherit (pkgs.vscode-extensions.nvarner) typst-lsp; });
    })

    (mkIf config.modules.develop.xdg.enable {
      # TODO:
    })
  ];
}
