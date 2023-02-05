{ config, options, lib, pkgs, ... }:

let
  inherit (lib) mkIf getExe;
  inherit (lib.my) mkBoolOpt;
in {
  options.modules.desktop.browsers.ungoogled = { enable = mkBoolOpt false; };

  config = mkIf config.modules.desktop.browsers.ungoogled.enable {
    user.packages = with pkgs;
      [
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
