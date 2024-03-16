{
  config,
  options,
  lib,
  pkgs,
  ...
}: let
  inherit (builtins) getEnv;
  inherit (lib.modules) mkIf;

  cfg = config.modules.shell.toolset.gnupg;
in {
  options.modules.shell.toolset.gnupg = let
    inherit (lib.options) mkEnableOption;
    inherit (lib.types) int;
    inherit (lib.my) mkOpt;
  in {
    enable = mkEnableOption "cryptographic suite";
    cacheTTL = mkOpt int 86400; # 24 hours
  };

  config = mkIf config.modules.shell.toolset.gnupg.enable {
    environment.variables.GNUPGHOME = "$XDG_CONFIG_HOME/gnupg";

    programs.gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
      pinentryPackage = pkgs.pinentry-gnome3;

      settings = {
        default-cache-ttl = cfg.cacheTTL;
        default-cache-ttl-ssh = cfg.cacheTTL;
        max-cache-ttl = cfg.cacheTTL;
        max-cache-ttl-ssh = cfg.cacheTTL;
      };
    };
  };
}
