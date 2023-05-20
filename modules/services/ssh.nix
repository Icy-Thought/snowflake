{
  options,
  config,
  lib,
  ...
}: let
  inherit (builtins) filter pathExists;
  inherit (lib.modules) mkIf;
in {
  options.modules.services.ssh = let
    inherit (lib.options) mkEnableOption;
  in {enable = mkEnableOption "secure-socket shell";};

  config = mkIf config.modules.services.ssh.enable {
    programs.ssh.startAgent = true;

    services.openssh = {
      enable = true;
      settings = {
        kbdInteractiveAuthentication = false;
        PasswordAuthentication = false;
        # startWhenNeeded = true;
      };

      hostKeys = [
        {
          comment = "icy-thought@host";
          path = "/etc/ssh/ed25519_key";
          rounds = 100;
          type = "ed25519";
        }
      ];
    };

    user.openssh.authorizedKeys.keyFiles =
      if config.user.name == "icy-thought"
      then
        filter pathExists [
          "${config.user.home}/.ssh/id_ed25519.pub"
          "${config.user.home}/.ssh/id_rsa.pub"
        ]
      else [];
  };
}
