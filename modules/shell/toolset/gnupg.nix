{ options, config, lib, pkgs, ... }:

let
  inherit (builtins) getEnv;
  inherit (lib.modules) mkIf;

  cfg = config.modules.shell.toolset.gnupg;
in {
  options.modules.shell.toolset.gnupg =
    let inherit (lib.options) mkEnableOption;
    in { enable = mkEnableOption "cryptographic suite"; };

  config = mkIf config.modules.shell.toolset.gnupg.enable {
    home.sessionVariables.GNUPGHOME = "$XDG_CONFIG_HOME/gnupg";

    programs.gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
      pinentryPackage = pkgs.pinentry-gtk2;

      settings = let cacheTTL = 86400;
      in {
        default-cache-ttl = cacheTTL;
        default-cache-ttl-ssh = cacheTTL;
        max-cache-ttl = cacheTTL;
        max-cache-ttl-ssh = cacheTTL;
      };
    };
  };
}
