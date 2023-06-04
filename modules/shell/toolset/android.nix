{
  config,
  options,
  lib,
  pkgs,
  ...
}: let
  inherit (lib.attrsets) attrValues;
  inherit (lib.modules) mkIf;
in {
  options.modules.shell.toolset.android = let
    inherit (lib.options) mkEnableOption;
  in {enable = mkEnableOption "android tools";};

  config = mkIf config.modules.shell.toolset.android.enable {
    user.packages = attrValues {inherit (pkgs) scrcpy;};

    # Android Debug Bridge
    programs.adb.enable = true;
    user.extraGroups = ["adbusers"];
  };
}
