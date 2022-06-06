{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.shell.bash;
in {
  options.modules.shell.bash = {
    enable = mkBoolOpt true;
  };

  config = mkIf cfg.enable (mkMerge [
    {
      home.programs.direnv = {
        enable = true;
        nix-direnv.enable = true;
        config.whitelist.prefix = ["/home"];
      };

      home.programs.bash = {
        enable = true;
        historySize = 5000;
        historyFileSize = 5000;
        historyIgnore = ["nvim" "neofetch"];

        shellAliases = mkMerge [
          {
            ls = "exa -Slhg --icons";
            lsa = "exa -Slhga --icons";

            wup = "systemctl start wg-quick-Akkadian-VPN.service";
            wud = "systemctl stop wg-quick-Akkadian-VPN.service";
          }

          (mkIf config.modules.desktop.editors.emacs.enable {
            temacs = "emacsclient -t";
          })
        ];
      };
    }

    # Starship intended for fish rice -> side-effect (hehe)
    (mkIf config.modules.shell.fish.enable {
      home.programs.bash.bashrcExtra = ''
        eval "$(starship init bash)"
      '';
    })
  ]);
}
