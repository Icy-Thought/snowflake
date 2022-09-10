{
  options,
  config,
  lib,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.services.ssh;
in {
  options.modules.services.ssh = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    programs.ssh.startAgent = true;

    services.openssh = {
      enable = true;
      kbdInteractiveAuthentication = false;
      passwordAuthentication = false;
      startWhenNeeded = true;
    };

    user.openssh.authorizedKeys.keys =
      if config.user.name == "icy-thought"
      then [
        # TODO: replace with functional ssh-key.
      ]
      else [];
  };
}
