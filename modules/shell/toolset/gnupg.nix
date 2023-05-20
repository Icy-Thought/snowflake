{
  config,
  options,
  lib,
  pkgs,
  ...
}: let
  inherit (lib.modules) mkIf;
in {
  options.modules.shell.gnupg = let
    inherit (lib.options) mkEnableOption;
    inherit (lib.types) int;
    inherit (lib.my) mkOpt;
  in {
    enable = mkEnableOption "cryptographic suite";
    cacheTTL = mkOpt int 3600; # 1hr
  };

  config = mkIf config.modules.shell.gnupg.enable {
    environment.variables = {GNUPGHOME = "$XDG_CONFIG_HOME/gnupg";};

    programs.gnupg.agent = {
      enable = true;
      # enableSSHSupport = true;
      # pinentryFlavor = "gtk2";
    };

    user.packages = [pkgs.tomb];

    # HACK Without this config file you get "No pinentry program" on 20.03.
    #      programs.gnupg.agent.pinentryFlavor doesn't appear to work, and this
    #      is cleaner than overriding the systemd unit.
    home.configFile.gpg-agent = {
      target = "gnupg/gpg-agent.conf";
      text = ''
        default-cache-ttl ${toString config.modules.shell.gnupg.cacheTTL}
        pinentry-program ${pkgs.pinentry.gtk2}/bin/pinentry
      '';
    };
  };
}
