{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.editors;
in {
  options.modules.desktop.editors = {
    default = mkOpt types.str "neovim";
  };

  config = mkMerge [
    (mkIf (cfg.default != null) {env.EDITOR = cfg.default;})

    (mkIf (cfg.emacs.enable || cfg.neovim.enable) {
      user.packages = with pkgs; [
        fd
        imagemagick
        (ripgrep.override {
          withPCRE2 = true;
        })
        editorconfig-core-c
        sqlite

        # module dependencies
        ## checkers: aspell
        (aspellWithDicts (ds:
          with ds; [
            en
            en-computers
            en-science
          ]))

        ## Markdown
        nodePackages.markdownlint-cli2
        vale

        ## lsp: LaTeX + Org-Mode
        tectonic
        # python310Packages.matplotlib
        # texlive.combined.scheme-medium
      ];
    })
  ];
}
