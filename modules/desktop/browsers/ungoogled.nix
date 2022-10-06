{ config
, options
, lib
, pkgs
, ...
}:
with lib;
with lib.my; {
  options.modules.desktop.browsers.unGoogled = {
    enable = mkBoolOpt false;
  };

  config = mkIf config.modules.desktop.browsers.unGoogled.enable {
    user.packages = with pkgs; [
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
      package = pkgs.ungoogled-chromium;
      extensions = [
        { id = "jhnleheckmknfcgijgkadoemagpecfol"; } # Auto-Tab-Discard
        { id = "nngceckbapebfimnlniiiahkandclblb"; } # Bitwarden
        { id = "dlnejlppicbjfcfcedcflplfjajinajd"; } # Bonjourr (New-Tab Page)
        { id = "eimadpbcbfnmbkopoojfekhnkhdbieeh"; } # Dark-Reader
        { id = "ldpochfccmkkmhdbclfhpagapcfdljkj"; } # Decentraleyes
        { id = "hlepfoohegkhhmjieoechaddaejaokhf"; } # Refined GitHub
        { id = "iaiomicjabeggjcfkbimgmglanimpnae"; } # Tab-Session-Manager
        { id = "cjpalhdlnbpafiamejdnhcphjbkeiagm"; } # Ublock-Origin
        { id = "jinjaccalgkegednnccohejagnlnfdag"; } # Violentmonkey
        {
          id = "dcpihecpambacapedldabdbpakmachpb";
          updateUrl = "https://raw.githubusercontent.com/iamadamdev/bypass-paywalls-chrome/master/src/updates/updates.xml";
        }
        (mkIf config.modules.desktop.gnome.enable [
          { id = "gphhapmejobijbbhgpjhcjognlahblep"; } # Gnome-Shell-Integration
        ])
      ];
    };
  };
}
