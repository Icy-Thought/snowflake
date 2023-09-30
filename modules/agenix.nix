{
  options,
  config,
  inputs,
  lib,
  pkgs,
  ...
}: let
  inherit (inputs) agenix;

  inherit (builtins) filter pathExists;
  inherit (lib.attrsets) mapAttrs' nameValuePair;
  inherit (lib.modules) mkDefault;
  inherit (lib.strings) removeSuffix;

  secretsDir = "${config.snowflake.hostDir}/secrets";
  secretsFile = "${secretsDir}/secrets.nix";
in {
  imports = [agenix.nixosModules.default];

  environment.systemPackages = [agenix.packages.x86_64-linux.default];

  age.secrets =
    if pathExists secretsFile
    then
      mapAttrs' (n: _:
        nameValuePair (removeSuffix ".age" n) {
          file = "${secretsDir}/${n}";
          owner = mkDefault config.user.name;
        }) (import secretsFile)
    else {};

  # age.identityPaths = options.age.identityPaths.default ++ (filter pathExists [
  #   "${config.user.home}/.ssh/id_ed25519"
  #   "${config.user.home}/.ssh/id_rsa"
  # ]);
}
