{ config, lib, pkgs, ... }: {

  programs.bash = {
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

}
