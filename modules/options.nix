{ config
, options
, lib
, home-manager
, ...
}:
with lib;
with lib.my; {
  options = with types; {
    user = mkOpt attrs { };

    snowflake = {
      dir = mkOpt path (findFirst pathExists (toString ../.) [
        "${config.user.home}/git/icy-thought/snowflake"
        "/etc/snowflake"
      ]);
      binDir = mkOpt path "${config.snowflake.dir}/bin";
      configDir = mkOpt path "${config.snowflake.dir}/config";
      modulesDir = mkOpt path "${config.snowflake.dir}/modules";
      themesDir = mkOpt path "${config.snowflake.modulesDir}/themes";
    };

    home = {
      file = mkOpt' attrs { } "Files to place directly in $HOME";
      configFile = mkOpt' attrs { } "Files to place in $XDG_CONFIG_HOME";
      dataFile = mkOpt' attrs { } "Files to place in $XDG_DATA_HOME";
      pointerCursor = mkOpt' attrs { } "Cursor to be applied on running system";
      activation = mkOpt' attrs { } "Script block to run after NixOS rebuild";
    };

    env = mkOption {
      type = attrsOf (oneOf [ str path (listOf (either str path)) ]);
      apply = mapAttrs (n: v:
        if isList v
        then concatMapStringsSep ":" (x: toString x) v
        else (toString v));
      default = { };
      description = "TODO";
    };
  };

  config = {
    user =
      let
        user = builtins.getEnv "USER";
        name =
          if elem user [ "" "root" ]
          then "icy-thought"
          else user;
      in
      {
        inherit name;
        description = "Primary user account";
        extraGroups = [ "wheel" ];
        isNormalUser = true;
        home = "/home/${name}";
        group = "users";
        uid = 1000;
      };

    # Necessary for nixos-rebuild build-vm to work.
    home-manager.useUserPackages = true;

    # Re-defining home-manager settings for modified option-names:
    # home.configFile  ->  home-manager.users.icy-thought.home.xdg.configFile
    # home.dataFile    ->  home-manager.users.icy-thought.home.xdg.dataFile
    hm.home = {
      activation = mkAliasDefinitions options.home.activation;
      file = mkAliasDefinitions options.home.file;
      pointerCursor = mkAliasDefinitions options.home.pointerCursor;
      stateVersion = config.system.stateVersion;
    };

    hm.xdg = {
      configFile = mkAliasDefinitions options.home.configFile;
      dataFile = mkAliasDefinitions options.home.dataFile;
    };

    users.users.${config.user.name} = mkAliasDefinitions options.user;

    nix.settings =
      let users = [ "root" config.user.name ];
      in {
        trusted-users = users;
        allowed-users = users;
      };

    env.PATH = [ "$SNOWFLAKE_BIN" "$XDG_BIN_HOME" "$PATH" ];

    environment.extraInit =
      concatStringsSep "\n"
        (mapAttrsToList (n: v: ''export ${n}="${v}"'') config.env);
  };
}
