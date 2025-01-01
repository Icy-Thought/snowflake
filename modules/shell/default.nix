{ options, config, lib, pkgs, ... }:

let cfg = config.modules.shell;
in with lib; {
  options.modules.shell = with types; {
    default = mkOption {
      type = nullOr (enum [ "fish" "zsh" ]);
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
      modules.shell.toolset = {
        lsd.enable = true;
        btop.enable = true;
        fzf.enable = true;
        gnupg.enable = true;
      };

      hm.programs.direnv = {
        enable = true;
        nix-direnv.enable = true;
        config.whitelist.prefix = [ "/home" ];
      };

      user.packages = with pkgs; [
        any-nix-shell
        pwgen
        yt-dlp
        ripdrag
        yazi

        # GNU Alternatives
        bat
        fd
        zoxide
        (pkgs.ripgrep.override { withPCRE2 = true; })
      ];
    })
  ];
}
