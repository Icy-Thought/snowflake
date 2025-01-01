{ options, config, lib, pkgs, ... }:

with lib; {
  options.modules.develop.scientific = {
    latex.enable = mkEnableOption "bloated doc/math lang";
    typst.enable = mkEnableOption "modern LaTeX alt.";
  };

  config = mkMerge [
    (mkIf config.modules.develop.scientific.latex.enable {
      user.packages = with pkgs; [
        texlab
        (texlive.combine {
          # :FIXME| completely replace with typst
          inherit (pkgs.texlive)
            scheme-basic capt-of dvipng dvisvgm fancyvrb fontspec hyperref
            latexmk ulem koma-script greek-inputenc trimspaces

            # :NOTE| Mathematics-related
            amsmath cancel mathtools

            # :NOTE| Graphics-related
            parskip pgf pgfplots svg transparent wrapfig xcolor;
        })
      ];
    })

    (mkIf config.modules.develop.scientific.typst.enable {
      user.packages = with pkgs; [ typst tinymist typstyle ];
    })

    (mkIf config.modules.develop.xdg.enable {
      # TODO:
    })
  ];
}
