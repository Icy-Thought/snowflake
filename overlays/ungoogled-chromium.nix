final: prev: {
  ungoogled-chromium = prev.ungoogled-chromium.override {
    commandLineArgs = toString [
      # Ungoogled flags
      "--disable-search-engine-collection"
      "--extension-mime-request-handling=always-prompt-for-install"
      "--popups-to-tabs"
      "--show-avatar-button=incognito-and-guest"

      # Experimental features
      "--enable-features=${
        final.lib.concatStringsSep "," [
          "BackForwardCache:enable_same_site/true"
          "CopyLinkToText"
          "OverlayScrollbar"
          "TabHoverCardImages"
          "VaapiVideoDecoder"
        ]
      }"

      # Dark mode
      "--force-dark-mode"

      # Performance
      "--enable-gpu-rasterization"
      "--enable-oop-rasterization"
      "--enable-zero-copy"
      "--ignore-gpu-blocklist"
    ];
  };
}
