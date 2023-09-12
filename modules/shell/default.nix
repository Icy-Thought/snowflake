{
  options,
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib.attrsets) attrValues;
  inherit (lib.modules) mkIf mkMerge;
  cfg = config.modules.shell;
in {
  options.modules.shell = let
    inherit (lib.options) mkOption mkEnableOption;
    inherit (lib.types) nullOr enum;
  in {
    default = mkOption {
      type = nullOr (enum ["fish" "zsh" "xonsh"]);
      default = null;
      description = "Default system shell";
    };
    corePkgs.enable = mkEnableOption "core shell packages";
  };

  config = mkMerge [
    (mkIf (cfg.default != null) {
      users.defaultUserShell = pkgs."${cfg.default}";
    })

    (mkIf cfg.corePkgs.enable {
      modules.shell.toolset.btop.enable = true;

      hm.programs.direnv = {
        enable = true;
        nix-direnv.enable = true;
        config.whitelist.prefix = ["/home"];
      };

      user.packages = attrValues {
        inherit (pkgs) any-nix-shell fzf pwgen yt-dlp csview ripdrag yazi;

        # GNU Alternatives
        inherit (pkgs) bat exa fd zoxide;
        rgFull = pkgs.ripgrep.override {withPCRE2 = true;};
      };
    })
  ];
}
