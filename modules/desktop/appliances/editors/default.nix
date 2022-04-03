{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.appliances.editors;
in {
  options.modules.desktop.appliances.editors = {
    default = mkOpt types.str "nvim";
  };

  config = (mkMerge [
    (mkIf (cfg.default != null) { env.EDITOR = cfg.default; })

    (mkIf (cfg.emacs.enable || cfg.neovim.enable) {
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
        ## lsp: haskell
        haskell-language-server
        stylish-haskell
        ## lsp: LaTeX
        tectonic
        gnuplot
        ## lsp: Org-Mode
        pandoc
        ## lsp: javascript
        nodePackages.typescript-language-server
        ## lsp: nix
        nixfmt
        rnix-lsp
        ## lsp: rust
        unstable.rust-analyzer
      ];
    })
  ]);
}
