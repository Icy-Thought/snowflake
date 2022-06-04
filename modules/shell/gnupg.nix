{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.shell.gnupg;
in {
  options.modules.shell.gnupg = with types; {
    enable = mkBoolOpt false;
    cacheTTL = mkOpt int 3600; # 1hr
  };

  config = mkIf cfg.enable {
    environment.variables = {
      GNUPGHOME = "$XDG_CONFIG_HOME/gnupg";
    };

    programs.gnupg.agent = {
      enable = true;
      pinentryFlavor = "gtk2";
    };

    user.packages = [pkgs.tomb];
  };
}
