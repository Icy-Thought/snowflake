{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.bash;
in {
  options.modules.shell.bash = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    homeManager.programs.bash = {
      enable = true;
      historyFileSize = 5000;
      historySize = 5000;
      historyIgnore = [ "nvim" ];

      shellAliases = {
        ls = "exa -Slhg --icons";
        lsa = "exa -Slhga --icons";
        temacs = "emacsclient -t";

        wup = "systemctl start wg-quick-Akkadian-VPN.service";
        wud = "systemctl stop wg-quick-Akkadian-VPN.service";
      };

      bashrcExtra = ''
        eval "$(starship init bash)"
      '';
    };
  };
}
