{ options
, config
, lib
, pkgs
, ...
}:
with lib;
with lib.my;

let cfg = config.modules.shell;
in {
  options.modules.shell = {
    default = mkOption {
      type = with types; nullOr (enum [ "fish" "zsh" "xonsh" ]);
      default = null;
      description = "Default system shell";
      example = "xonsh";
    };
    usefulPkgs.enable = mkBoolOpt false;
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

      user.packages = with pkgs; [
        any-nix-shell
        fzf
        pwgen
        yt-dlp
        csview
        ripdrag

        # GNU Alternatives
        bat
        exa
        fd
        (ripgrep.override { withPCRE2 = true; })
        zoxide
      ];
    })
  ];
}
