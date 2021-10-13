{ options, config, lib, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.ssh;
in {
  options.modules.services.ssh = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    services.openssh = {
      enable = true;
      challengeResponseAuthentication = false;
      passwordAuthentication = false;
      startWhenNeeded = true;
    };

    programs.ssh.startAgent = true;

    user.openssh.authorizedKeys.keys = if config.user.name == "icy-thought" then
      [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIE0MHljC7KVnv+K2nyvGGY+Yu2Gst0lNx7jdRmiez5o1 sirius@NixOS"
      ]
    else
      [ ];
  };
}
