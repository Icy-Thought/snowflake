{ options, config, lib, pkgs, ... }:

let cfg = config.modules.shell.toolset.gnupg;
in with lib; {
  options.modules.shell.toolset.gnupg = {
    enable = mkEnableOption "cryptographic suite";
  };

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
