{ inputs, options, config, lib, pkgs, ... }:

with lib; {
  options.modules.desktop.browsers.zen = {
    enable = mkEnableOption "Firefox based browser";
  };

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
          exec = "${getExe zen-browser} --private-window";
          categories = [ "Network" "WebBrowser" ];
        })
      ];
  };
}
