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
    hm.programs.vscode = with config.snowflake; {
      enable = true;
      package = pkgs.vscodium;
      mutableExtensionsDir = true;

      extensions = import "${configDir}/vscodium/extensions.nix".extensions {
        inherit pkgs;
      };

      userSettings =
        import "${configDir}/vscodium/settings.nix" {inherit config;};
      keybindings = import "${configDir}/vscodium/keybindings.nix" {};
    };
  };
}
