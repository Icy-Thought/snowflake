{ config, options, lib, home-manager, ... }:

let
  inherit (lib.modules) mkAliasDefinitions;
  inherit (lib.my) mkOpt mkOpt';
in {
  options = let inherit (lib.types) attrs path;
  in {
    user = mkOpt attrs { };

    snowflake = {
      dir = mkOpt path
        (lib.findFirst builtins.pathExists (builtins.toString ../.) [
          "${config.user.home}/Workspace/public/snowflake"
          "/etc/snowflake"
        ]);
      hostDir = mkOpt path
        "${config.snowflake.dir}/hosts/${config.networking.hostName}";
      binDir = mkOpt path "${config.snowflake.dir}/bin";
      configDir = mkOpt path "${config.snowflake.dir}/config";
      modulesDir = mkOpt path "${config.snowflake.dir}/modules";
      themesDir = mkOpt path "${config.snowflake.modulesDir}/themes";
    };
  };

  config = {
    user = let
      user = builtins.getEnv "USER";
      name = if builtins.elem user [ "" "root" ] then "icy-thought" else user;
    in {
      inherit name;
      description = "Primary user account";
      extraGroups = [ "wheel" "input" "audio" "video" "storage" ];
      isNormalUser = true;
      home = "/home/${name}";
      group = "users";
      uid = 1000;
    };

    # Necessary for nixos-rebuild build-vm to work.
    home-manager.useUserPackages = true;

    home = {
      stateVersion = config.system.stateVersion;
      sessionPath = [ "$SNOWFLAKE_BIN" "$XDG_BIN_HOME" "$PATH" ];
    };

    users.users.${config.user.name} = mkAliasDefinitions options.user;

    nix.settings = let users = [ "root" config.user.name ];
    in {
      trusted-users = users;
      allowed-users = users;
    };
  };
}
