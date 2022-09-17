{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.shell;
in {
  options.modules.shell = {
    default = mkOption {
      type = with types; package;
      default = pkgs.fish;
      description = "Default system shell";
      example = "bash";
    };
    usefulPkgs.enable = mkBoolOpt false;
  };

  config = mkMerge [
    (mkIf (cfg.default != null) {
      users.defaultUserShell = cfg.default;
    })

    (mkIf cfg.usefulPkgs.enable {
      user.packages = with pkgs; [
        any-nix-shell
        fzf
        pwgen
        yt-dlp
        csview

        # GNU Alternatives
        bat
        bottom
        exa
        fd
        (ripgrep.override {withPCRE2 = true;})
        zoxide
      ];
    })
  ];
}
