{ options
, config
, lib
, pkgs
, ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.browsers.firefox;
in
{
  options.modules.desktop.browsers.firefox = with types; {
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

  config = mkIf cfg.enable (mkMerge [
    {
      user.packages = with pkgs; [
        firefox-devedition-bin
        (makeDesktopItem {
          name = "firefox-devedition-private";
          desktopName = "Firefox-DevEdition (Private)";
          genericName = "Launch a Private Firefox-DevEdition Instance";
          icon = "firefox";
          exec = "${firefox-devedition-bin}/bin/firefox-devedition --private-window";
          categories = [ "Network" "WebBrowser" ];
        })
      ];

      # Prevent auto-creation of ~/Desktop. The trailing slash is necessary.
      # See: https://bugzilla.mozilla.org/show_bug.cgi?id=1082717
      env.XDG_DESKTOP_DIR = "$HOME/";

      modules.desktop.browsers.firefox.settings = {
        # Enables dark-themed flash before page-load:
        "ui.systemUsesDarkTheme" = "1";
        # Developer tools -> uses dark theme
        "devtools.theme" = "dark";
        # Enables ETP = decent security -> firefox containers = redundent
        "browser.contentblocking.category" = "strict";
        "privacy.donottrackheader.enabled" = true;
        "privacy.donottrackheader.value" = 1;
        "privacy.purge_trackers.enabled" = true;
        # Syncs Firefox toolbar settings across machines
        # WARNING: May not work across OS'es
        "services.sync.prefs.sync.browser.uiCustomization.state" = true;
        # Enables userContent.css and userChrome.css for our theme modules
        "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
        # Stop creating ~/Downloads!
        "browser.download.dir" = "${config.user.home}/downloads";
        # Disables built-in password manager -> use external PM!
        "signon.rememberSignons" = false;
        # Firefox, DO NOT CHECK if you are the default browser..
        "browser.shell.checkDefaultBrowser" = false;
        # Disables "New Tab Page" feature
        "browser.newtabpage.enabled" = false;
        # Disables Activity Stream
        "browser.newtabpage.activity-stream.enabled" = false;
        "browser.newtabpage.activity-stream.telemetry" = false;
        # Disables new tab tile ads & preload
        "browser.newtabpage.enhanced" = false;
        "browser.newtabpage.introShown" = true;
        "browser.newtab.preload" = false;
        "browser.newtabpage.directory.ping" = "";
        "browser.newtabpage.directory.source" = "data:text/plain,{}";
        # Reduces search engine noise in the urlbar's completion window
        # PS: Shortcuts and suggestions still work
        "browser.urlbar.suggest.searches" = false;
        "browser.urlbar.shortcuts.bookmarks" = false;
        "browser.urlbar.shortcuts.history" = false;
        "browser.urlbar.shortcuts.tabs" = false;
        "browser.urlbar.showSearchSuggestionsFirst" = false;
        "browser.urlbar.speculativeConnect.enabled" = false;
        # Prevents search terms from being sent to ISP
        "browser.urlbar.dnsResolveSingleWordsAfterSearch" = 0;
        # Disables sponsored search results
        "browser.urlbar.suggest.quicksuggest.nonsponsored" = false;
        "browser.urlbar.suggest.quicksuggest.sponsored" = false;
        # Shows whole URL in address bar
        "browser.urlbar.trimURLs" = false;
        # Disables non-useful funcionality of certain features
        "browser.disableResetPrompt" = true;
        "browser.onboarding.enabled" = false;
        "browser.aboutConfig.showWarning" = false;
        "media.videocontrols.picture-in-picture.video-toggle.enabled" = false;
        "extensions.pocket.enabled" = false;
        "extensions.shield-recipe-client.enabled" = false;
        "reader.parse-on-load.enabled" = false;

        # Security-oriented defaults:
        "security.family_safety.mode" = 0;
        # https://blog.mozilla.org/security/2016/10/18/phasing-out-sha-1-on-the-public-web/
        "security.pki.sha1_enforcement_level" = 1;
        # https://github.com/tlswg/tls13-spec/issues/1001
        "security.tls.enable_0rtt_data" = false;
        # Uses Mozilla geolocation service instead of Google if given permission
        "geo.provider.network.url" = "https://location.services.mozilla.com/v1/geolocate?key=%MOZILLA_API_KEY%";
        "geo.provider.use_gpsd" = false;
        # https://support.mozilla.org/en-US/kb/extension-recommendations
        "browser.newtabpage.activity-stream.asrouter.userprefs.cfr" = false;
        "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.addons" =
          false;
        "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.features" =
          false;
        "extensions.htmlaboutaddons.recommendations.enabled" = false;
        "extensions.htmlaboutaddons.discover.enabled" = false;
        "extensions.getAddons.showPane" = false; # Uses Google Analytics
        "browser.discovery.enabled" = false;
        # Reduces File IO / SSD abuse, 15 seconds -> 30 minutes
        "browser.sessionstore.interval" = "1800000";
        # Disables battery API
        "dom.battery.enabled" = false;
        # Disables "beacon" asynchronous HTTP transfers (used for analytics)
        "beacon.enabled" = false;
        # Disables pinging URIs specified in HTML <a> ping= attributes
        "browser.send_pings" = false;
        # Disables gamepad API to prevent USB device enumeration
        "dom.gamepad.enabled" = false;
        # Prevents guessing domain names on invalid entry in URL-bar
        "browser.fixup.alternate.enabled" = false;
        # Disables telemetry settings
        "toolkit.telemetry.unified" = false;
        "toolkit.telemetry.enabled" = false;
        "toolkit.telemetry.server" = "data:,";
        "toolkit.telemetry.archive.enabled" = false;
        "toolkit.telemetry.coverage.opt-out" = true;
        "toolkit.coverage.opt-out" = true;
        "toolkit.coverage.endpoint.base" = "";
        "experiments.supported" = false;
        "experiments.enabled" = false;
        "experiments.manifest.uri" = "";
        "browser.ping-centre.telemetry" = false;
        # https://mozilla.github.io/normandy/
        "app.normandy.enabled" = false;
        "app.normandy.api_url" = "";
        "app.shield.optoutstudies.enabled" = false;
        # Disables health reports (basically more telemetry)
        "datareporting.healthreport.uploadEnabled" = false;
        "datareporting.healthreport.service.enabled" = false;
        "datareporting.policy.dataSubmissionEnabled" = false;
        # Disables crash reports
        "breakpad.reportURL" = "";
        "browser.tabs.crashReporting.sendReport" = false;
        # Prevents the submission of backlogged reports
        "browser.crashReports.unsubmittedCheck.autoSubmit2" = false;

        # Disable automatic Form autofill
        "browser.formfill.enable" = false;
        "extensions.formautofill.addresses.enabled" = false;
        "extensions.formautofill.available" = "off";
        "extensions.formautofill.creditCards.available" = false;
        "extensions.formautofill.creditCards.enabled" = false;
        "extensions.formautofill.heuristics.enabled" = false;
      };

      # Use a stable profile name so we can target it in themes
      home.file =
        let
          cfgPath = ".mozilla/firefox";
        in
        {
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

          "${cfgPath}/${cfg.profileName}.dev-edition-default/user.js" = mkIf (cfg.settings != { } || cfg.extraConfig != "") {
            text = ''
              ${concatStrings (mapAttrsToList (name: value: ''
                  user_pref("${name}", ${builtins.toJSON value});
                '')
                cfg.settings)}
              ${cfg.extraConfig}
            '';
          };

          "${cfgPath}/${cfg.profileName}.dev-edition-default/chrome/userChrome.css" =
            mkIf (cfg.userChrome != "") { text = cfg.userChrome; };

          "${cfgPath}/${cfg.profileName}.dev-edition-default/chrome/userContent.css" =
            mkIf (cfg.userContent != "") { text = cfg.userContent; };
        };
    }
  ]);
}
