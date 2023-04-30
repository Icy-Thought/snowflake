{ config, options, lib, pkgs, ... }:

let
  inherit (builtins) toString;
  inherit (lib.meta) getExe;
  inherit (lib.modules) mkIf;
  inherit (lib.strings) concatStringsSep;
in {
  options.modules.desktop.browsers.ungoogled =
    let inherit (lib.options) mkEnableOption;
    in { enable = mkEnableOption false; };

  config = mkIf config.modules.desktop.browsers.ungoogled.enable {
    user.packages = let inherit (pkgs) makeDesktopItem ungoogled-chromium;
    in [
      (makeDesktopItem {
        name = "ungoogled-private";
        desktopName = "Ungoogled Web Browser (Private)";
        genericName = "Launch a Private Ungoogled Chromium Instance";
        icon = "chromium";
        exec = "${getExe ungoogled-chromium} --incognito";
        categories = [ "Network" ];
      })
    ];

    hm.programs.chromium = {
      enable = true;
      package = let
        ungoogledFlags = toString [
          "--force-dark-mode"
          "--disable-search-engine-collection"
          "--extension-mime-request-handling=always-prompt-for-install"
          "--fingerprinting-canvas-image-data-noise"
          "--fingerprinting-canvas-measuretext-noise"
          "--fingerprinting-client-rects-noise"
          "--popups-to-tabs"
          "--show-avatar-button=incognito-and-guest"

          # Performance
          "--enable-gpu-rasterization"
          "--enable-oop-rasterization"
          "--enable-zero-copy"
          "--ignore-gpu-blocklist"

          # Experimental features
          "--enable-features=${
            concatStringsSep "," [
              "BackForwardCache:enable_same_site/true"
              "CopyLinkToText"
              "OverlayScrollbar"
              "TabHoverCardImages"
              "VaapiVideoDecoder"
            ]
          }"
        ];
      in pkgs.ungoogled-chromium.override {
        commandLineArgs = [ ungoogledFlags ];
      };
      extensions = [
        { id = "jhnleheckmknfcgijgkadoemagpecfol"; } # Auto-Tab-Discard
        { id = "nngceckbapebfimnlniiiahkandclblb"; } # Bitwarden
        { id = "dlnejlppicbjfcfcedcflplfjajinajd"; } # Bonjourr (New-Tab Page)
        { id = "eimadpbcbfnmbkopoojfekhnkhdbieeh"; } # Dark-Reader
        { id = "ldpochfccmkkmhdbclfhpagapcfdljkj"; } # Decentraleyes
        { id = "bkdgflcldnnnapblkhphbgpggdiikppg"; } # DuckDuckGo
        { id = "hlepfoohegkhhmjieoechaddaejaokhf"; } # Refined GitHub
        { id = "iaiomicjabeggjcfkbimgmglanimpnae"; } # Tab-Session-Manager
        { id = "cjpalhdlnbpafiamejdnhcphjbkeiagm"; } # Ublock-Origin
        { id = "dbepggeogbaibhgnhhndojpepiihcmeb"; } # Vimium
        { id = "jinjaccalgkegednnccohejagnlnfdag"; } # Violentmonkey
        {
          id = "dcpihecpambacapedldabdbpakmachpb";
          updateUrl =
            "https://raw.githubusercontent.com/iamadamdev/bypass-paywalls-chrome/master/src/updates/updates.xml";
        }
        (mkIf config.modules.desktop.gnome.enable [{
          id = "gphhapmejobijbbhgpjhcjognlahblep";
        } # Gnome-Shell-Integration
          ])
      ];
    };
  };
}
