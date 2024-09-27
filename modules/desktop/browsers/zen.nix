{ inputs, config, options, lib, pkgs, ... }:

let
  inherit (lib.attrsets) attrValues;
  inherit (lib.modules) mkIf;
in {
  options.modules.desktop.browsers.zen =
    let inherit (lib.options) mkEnableOption;
    in { enable = mkEnableOption "Firefox based browser"; };

  config = mkIf config.modules.desktop.browsers.zen.enable {

    user.packages =
      let zen-browser = inputs.zen-browser.packages."${pkgs.system}".default;
      in [
        zen-browser
        (pkgs.makeDesktopItem {
          name = "zen-private";
          desktopName = "Zen Browser (Private)";
          genericName = "Launch a private Zen Browser instance";
          icon = "zen";
          exec = "${lib.getExe zen-browser} --private-window";
          categories = [ "Network" "WebBrowser" ];
        })
      ];

    environment.variables = mkIf (config.modules.desktop.type == "wayland") {
      MOZ_ENABLE_WAYLAND = "1";
    };
  };
}
