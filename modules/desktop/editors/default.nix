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
    default = mkOption {
      type = with types; str;
      default = "nvim";
      description = "Default editor";
      example = "emacs";
    };
  };

  config = mkMerge [
    (mkIf (cfg.default != null) {
      env.EDITOR = cfg.default;
    })

    (mkIf (cfg.default == "nvim" || cfg.default == "emacs") {
      user.packages = with pkgs; [
        imagemagick
        editorconfig-core-c
        sqlite

        # module dependencies
        ## checkers: aspell
        (aspellWithDicts (dct:
          with dct; [
            en
            en-computers
            en-science
          ]))

        ## Markdown
        nodePackages.markdownlint-cli2
        vale

        ## lsp: LaTeX + Org-Mode
        tectonic
        pandoc
      ];
    })
  ];
}
