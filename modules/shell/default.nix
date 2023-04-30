{ options, config, lib, pkgs, ... }:

let
  inherit (lib.attrsets) attrValues;
  inherit (lib.modules) mkIf mkMerge;
  cfg = config.modules.shell;
in {
  options.modules.shell = let
    inherit (lib.options) mkOption mkEnableOption;
    inherit (lib.types) nullOr enum;
  in {
    default = mkOption {
      type = nullOr (enum [ "fish" "zsh" "xonsh" ]);
      default = null;
      description = "Default system shell";
    };
    usefulPkgs.enable = mkEnableOption false;
  };

  config = mkMerge [
    (mkIf (cfg.default != null) {
      users.defaultUserShell = pkgs."${cfg.default}";
    })

    (mkIf cfg.usefulPkgs.enable {
      modules.shell.btop.enable = true;

      hm.programs.direnv = {
        enable = true;
        nix-direnv.enable = true;
        config.whitelist.prefix = [ "/home" ];
      };

      user.packages = attrValues ({
        inherit (pkgs) any-nix-shell fzf pwgen yt-dlp csview ripdrag;

        # GNU Alternatives
        inherit (pkgs) bat exa fd zoxide;
        rgFull = pkgs.ripgrep.override { withPCRE2 = true; };
      });
    })
  ];
}
