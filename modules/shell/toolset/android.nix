{ options, config, lib, pkgs, ... }:

with lib; {
  options.modules.shell.toolset.android = {
    enable = mkEnableOption "android tools";
  };

  config = mkIf config.modules.shell.toolset.android.enable {
    user.packages = [ pkgs.scrcpy ];

    # Android Debug Bridge
    programs.adb.enable = true;
    user.extraGroups = [ "plugdev" ];
  };
}
