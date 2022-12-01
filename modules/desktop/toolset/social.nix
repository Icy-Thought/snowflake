{ config
, options
, lib
, pkgs
, ...
}:
with lib;
with lib.my;

let cfg = config.modules.desktop.toolset.social;
in {
  options.modules.desktop.toolset.social = {
    common.enable = mkBoolOpt true;
  };

  config = mkMerge [
    (mkIf cfg.common.enable {
      user.packages = with pkgs;
        let
          # Copyright (c) 2022 roosemberth. All Rights Reserved.
          element-desktop' = pkgs.symlinkJoin {
            name = "element-desktop-in-dataHome";
            paths = [ element-desktop ];
            nativeBuildInputs = [ pkgs.makeWrapper ];
            postBuild = ''
              wrapProgram "$out/bin/element-desktop" \
                --add-flags '--profile-dir $XDG_DATA_HOME/Element'
            '';
          };
        in
        [
          element-desktop'
          unstable.discord
          signal-desktop
          tdesktop
        ];
    })
  ];
}
