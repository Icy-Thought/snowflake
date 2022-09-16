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

  config = mkIf cfg.enable {
    hm.programs.direnv = {
      enable = true;
      nix-direnv.enable = true;
      config.whitelist.prefix = ["/home"];
    };

    # Enable starship-rs:
    modules.shell.starship.enable = true;
    hm.programs.starship.enableBashIntegration = true;

    hm.programs.bash = {
      enable = true;
      historySize = 5000;
      historyFileSize = 5000;
      historyIgnore = ["btm" "htop" "macchina" "neofetch"];
      shellAliases = {
        ls = "exa -Slhg --icons";
        lsa = "exa -Slhga --icons";
        wup = "systemctl start wg-quick-Akkadian-VPN.service";
        wud = "systemctl stop wg-quick-Akkadian-VPN.service";
      };
      bashrcExtra = ''
        eval "$(starship init bash)"
      '';
    };
  };
}
