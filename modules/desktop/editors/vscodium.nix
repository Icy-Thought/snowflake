{
  config,
  options,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.editors.vscodium;
in {
  options.modules.desktop.editors.vscodium = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    home.programs.vscode = with config.snowflake; {
      enable = true;
      package = pkgs.vscodium;
      mutableExtensionsDir = true;

      extensions = import "${configDir}/vscodium/extensions.nix" {
        inherit config;
        inherit pkgs;
      };

      userSettings =
        import "${configDir}/vscodium/settings.nix" {inherit config;};
      keybindings = import "${configDir}/vscodium/keybindings.nix" {};
    };
  };
}
