{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.editors;
in {
  options.modules.desktop.editors = { default = mkOpt types.str "nvim"; };

  config = (mkMerge [
    (mkIf (cfg.default != null) { env.EDITOR = cfg.default; })

    (mkIf (cfg.emacs.enable || cfg.nvim.enable) {
      user.packages = with pkgs; [
        #extraPkgs
        fd
        imagemagick
        (ripgrep.override { withPCRE2 = true; })
        # toolbox
        editorconfig-core-c

        # module dependencies
        ## checkers: aspell
        (aspellWithDicts (ds: with ds; [ en en-computers en-science ]))
        ## lsp: LaTeX + Org-Mode
        gnuplot
        pandoc
        tectonic
      ];
    })
  ]);
}
