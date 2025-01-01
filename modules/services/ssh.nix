{ options, config, lib, ... }:

with lib; {
  options.modules.services.ssh = {
    enable = mkEnableOption "secure-socket shell";
  };

  config = mkIf config.modules.services.ssh.enable {
    services.openssh = {
      enable = true;
      settings = {
        KbdInteractiveAuthentication = false;
        PasswordAuthentication = false;
      };

      hostKeys = [{
        comment = "icy-thought@host";
        path = "/etc/ssh/ed25519_key";
        rounds = 100;
        type = "ed25519";
      }];
    };

    user.openssh.authorizedKeys.keyFiles =
      if config.user.name == "icy-thought" then
        builtins.filter builtins.pathExists [
          "${config.user.home}/.ssh/id_ed25519.pub"
          "${config.user.home}/.ssh/id_rsa.pub"
        ]
      else
        [ ];
  };
}
