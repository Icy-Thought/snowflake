{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.bash;
in {
  options.modules.shell.bash = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    programs.bash.enable = true;
    programs.bash.historyFileSize = 5000;
    programs.bash.historySize = 5000;
    programs.bash.historyIgnore = [ "nvim" ];

    programs.bash.shellAliases = {
      ls = "exa -Slhg --icons";
      lsa = "exa -Slhga --icons";
      temacs = "emacsclient -t";

      wup = "systemctl start wg-quick-Akkadian-VPN.service";
      wud = "systemctl stop wg-quick-Akkadian-VPN.service";
    };

    programs.bash.bashrcExtra = ''
      eval "$(starship init bash)"
    '';
  };
}
