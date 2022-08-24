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
      user.packages = [
        pkgs.imagemagick
        pkgs.editorconfig-core-c
        pkgs.sqlite

        # module dependencies
        ## checkers: aspell
        (pkgs.aspellWithDicts (dict: [
          dict.en
          dict.en-computers
          dict.en-science
        ]))

        ## Markdown
        pkgs.nodePackages.markdownlint-cli2
        pkgs.vale

        ## lsp: LaTeX + Org-Mode
        pkgs.pandoc
        pkgs.tectonic
        pkgs.texlab
      ];
    })
  ];
}
