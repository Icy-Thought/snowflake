{ config
, options
, lib
, pkgs
, ...
}:
with lib;
with lib.my; {
  options.modules.shell.bash = {
    enable = mkBoolOpt true;
  };

  config = mkIf config.modules.shell.bash.enable {
    # Enable starship-rs:
    modules.shell.starship.enable = true;
    hm.programs.starship.enableBashIntegration = true;

    hm.programs.bash = {
      enable = true;
      historySize = 5000;
      historyFileSize = 5000;
      historyIgnore = [ "btm" "htop" "macchina" "neofetch" ];
      shellAliases = {
        ls = "exa -Slhg --icons";
        lsa = "exa -Slhga --icons";
        wup = "systemctl start wg-quick-Akkadian-VPN.service";
        wud = "systemctl stop wg-quick-Akkadian-VPN.service";
      };
      bashrcExtra = ''
        eval "$(starship init bash)"
        eval "$(direnv hook bash)"
      '';
    };
  };
}
