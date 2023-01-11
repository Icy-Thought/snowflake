{ options
, config
, lib
, ...
}:
with lib;
with lib.my; {
  options.modules.services.ssh = {
    enable = mkBoolOpt false;
  };

  config = mkIf config.modules.services.ssh.enable {
    programs.ssh.startAgent = true;

    services.openssh = {
      enable = true;
      kbdInteractiveAuthentication = false;
      passwordAuthentication = false;
      # startWhenNeeded = true;

      hostKeys = [{
        comment = "icy-thought@host";
        path = "/etc/ssh/ed25519_key";
        rounds = 100;
        type = "ed25519";
      }];
    };

    user.openssh.authorizedKeys.keyFiles =
      if config.user.name == "icy-thought" then
        filter pathExists [
          "${config.user.home}/.ssh/id_ed25519.pub"
          "${config.user.home}/.ssh/id_rsa.pub"
        ]
      else [ ];
  };
}
