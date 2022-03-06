{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.appliances.browsers.firefox;
in {
  options.modules.desktop.appliances.browsers.firefox = with types; {
    enable = mkBoolOpt false;
    profileName = mkOpt types.str config.user.name;

    settings = mkOpt' (attrsOf (oneOf [ bool int str ])) { } ''
      Firefox preferences set in <filename>user.js</filename>
    '';
    extraConfig = mkOpt' lines "" ''
      Extra lines to add to <filename>user.js</filename>
    '';

    userChrome = mkOpt' lines "" "CSS Styles for Firefox's interface";
    userContent = mkOpt' lines "" "Global CSS Styles for websites";
  };

  config = mkIf cfg.enable (mkMerge [{
    user.packages = with pkgs; [
      firefox-devedition-bin
      (makeDesktopItem {
        name = "firefox-devedition-private";
        desktopName = "Firefox-DevEdition (Private)";
        genericName = "Open a Private Firefox-DevEdition Window";
        icon = "firefox";
        exec =
          "${firefox-devedition-bin}/bin/firefox-devedition --private-window";
        categories = [ "Network" ];
      })
    ];

    # Prevent auto-creation of ~/Desktop. The trailing slash is necessary.
    # See: https://bugzilla.mozilla.org/show_bug.cgi?id=1082717
    env.XDG_DESKTOP_DIR = "$HOME/";

    modules.desktop.appliances.browsers.firefox.settings = {
      # Enable dark-themed flash before page-load:
      "ui.systemUsesDarkTheme" = "1";
      # Enable dark-theming for developer tools:
      "devtools.theme" = "dark";
      # Enable userContent.css and userChrome.css for our theme modules:
      "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
      # Don't use the built-in password manager:
      "signon.rememberSignons" = false;
      # Do not check if Firefox is the default browser..:
      "browser.shell.checkDefaultBrowser" = false;
      # Disable Activity Stream:
      "browser.newtabpage.activity-stream.enabled" = false;
      # Disable new tab tile ads & preload:
      "browser.newtabpage.enhanced" = false;
      "browser.newtab.preload" = false;
      "browser.newtabpage.directory.ping" = "";
      "browser.newtabpage.directory.source" = "data:text/plain,{}";
      # Disable non-useful crap:
      "extensions.htmlaboutaddons.recommendations.enabled" = false;
      "extensions.htmlaboutaddons.discover.enabled" = false;
      "extensions.pocket.enabled" = false;
      "app.normandy.enabled" = false;
      "app.normandy.api_url" = "";
      "extensions.shield-recipe-client.enabled" = false;
      "app.shield.optoutstudies.enabled" = false;
      # Disable battery API:
      "dom.battery.enabled" = false;
      # Disable "beacon" asynchronous HTTP transfers: (used for analytics)
      "beacon.enabled" = false;
      # Disable pinging URIs specified in HTML <a> ping = attributes:
      "browser.send_pings" = false;
      # Disable gamepad API to prevent USB device enumeration:
      "dom.gamepad.enabled" = false;
      # Don't try to guess domain names when entering an invalid domain name in URL bar:
      "browser.fixup.alternate.enabled" = false;
      # Disable sponsored site suggestions:
      "browser.urlbar.suggest.quicksuggest.sponsored.enabled" = false;
      # Disable telemetry:
      "toolkit.telemetry.enabled" = false;
      "toolkit.telemetry.unified" = false;
      "toolkit.telemetry.archive.enabled" = false;
      "experiments.supported" = false;
      "experiments.enabled" = false;
      "experiments.manifest.uri" = "";
      # Disable health reports (basically more telemetry)
      "datareporting.healthreport.uploadEnabled" = false;
      "datareporting.healthreport.service.enabled" = false;
      "datareporting.policy.dataSubmissionEnabled" = false;
    };

    # Use a stable profile name so we can target it in themes
    home.file = let cfgPath = ".mozilla/firefox";
    in {
      "${cfgPath}/profiles.ini".text = ''
        [Profile0]
        Name=dev-edition-default
        IsRelative=1
        Path=${cfg.profileName}.dev-edition-default
        Default=1

        [General]
        StartWithLastProfile=1
        Version=2
      '';

      "${cfgPath}/${cfg.profileName}.dev-edition-default/user.js" =
        mkIf (cfg.settings != { } || cfg.extraConfig != "") {
          text = ''
            ${concatStrings (mapAttrsToList (name: value: ''
              user_pref("${name}", ${builtins.toJSON value});
            '') cfg.settings)}
            ${cfg.extraConfig}
          '';
        };

      "${cfgPath}/${cfg.profileName}.dev-edition-default/chrome/userChrome.css" =
        mkIf (cfg.userChrome != "") { text = cfg.userChrome; };

      "${cfgPath}/${cfg.profileName}.dev-edition-default/chrome/userContent.css" =
        mkIf (cfg.userContent != "") { text = cfg.userContent; };
    };
  }]);
}
