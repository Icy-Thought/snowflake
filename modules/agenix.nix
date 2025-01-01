{ inputs, options, config, lib, pkgs, ... }:

let
  secretsDir = "${config.snowflake.hostDir}/secrets";
  secretsFile = "${secretsDir}/secrets.nix";
in with lib; {
  imports = [ inputs.ragenix.nixosModules.default ];

  environment.systemPackages = [ inputs.ragenix.packages.x86_64-linux.default ];

  age.secrets = if builtins.pathExists secretsFile then
    mapAttrs' (n: _:
      nameValuePair (removeSuffix ".age" n) {
        file = "${secretsDir}/${n}";
        owner = mkDefault config.user.name;
      }) (import secretsFile)
  else
    { };

  # age.identityPaths = options.age.identityPaths.default ++ (builtins.filter builtins.pathExists [
  #   "${config.user.home}/.ssh/id_ed25519"
  #   "${config.user.home}/.ssh/id_rsa"
  # ]);
}
