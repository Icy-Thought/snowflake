{ config, options, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
  inherit (lib.my) mkBoolOpt;
in {
  options.modules.shell.bash = { enable = mkBoolOpt true; };

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
        less = "less -R";
        wup = "systemctl start wg-quick-Akkadian-VPN.service";
        wud = "systemctl stop wg-quick-Akkadian-VPN.service";
      };
      bashrcExtra = ''
        # -------===[ Useful Functions ]===------- #
        function sys_update {
            nixos-rebuild switch --use-remote-sudo --flake .#"$(hostname)" --impure
        }

        # -------===[ External Plugins ]===------- #
        eval "$(starship init bash)"
        eval "$(direnv hook bash)"
      '';
    };
  };
}
