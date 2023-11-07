{
  config,
  options,
  lib,
  pkgs,
  ...
}: let
  inherit (lib.modules) mkIf;
in {
  options.modules.shell.bash = let
    inherit (lib.options) mkEnableOption;
  in {enable = mkEnableOption "bash shell" // {default = true;};};

  config = mkIf config.modules.shell.bash.enable {
    # Enable starship-rs + ZSH integration
    modules.shell.toolset.starship.enable = true;
    hm.programs.starship.enableBashIntegration = true;

    hm.programs.bash = {
      enable = true;
      historySize = 5000;
      historyFileSize = 5000;
      historyIgnore = ["btm" "htop" "macchina" "neofetch"];
      shellAliases = {
        ls = "lsd -Sl";
        lsa = "lsd -Sla";
        less = "less -R";
        wup = "systemctl start wg-quick-Akkadian-VPN.service";
        wud = "systemctl stop wg-quick-Akkadian-VPN.service";
      };
      bashrcExtra = ''
        # -------===[ Useful Functions ]===------- #
        function sysdate {
            nixos-rebuild switch --use-remote-sudo --flake .#"$(hostname)" --impure
        }

        # -------===[ External Plugins ]===------- #
        eval "$(starship init bash)"
        eval "$(direnv hook bash)"
      '';
    };
  };
}
