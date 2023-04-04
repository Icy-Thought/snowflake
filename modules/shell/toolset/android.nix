{ config, options, lib, pkgs, ... }:

let
  inherit (lib) mkIf attrValues;
  inherit (lib.my) mkBoolOpt;
in {
  options.modules.shell.android = { enable = mkBoolOpt false; };

  config = mkIf config.modules.shell.android.enable {
    user.packages = attrValues ({ inherit (pkgs) scrcpy; });

    # Android Debug Bridge
    programs.adb.enable = true;
    user.extraGroups = [ "adbusers" ];
  };
}
