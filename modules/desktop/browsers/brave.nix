{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.browsers.brave;
in {
  options.modules.desktop.browsers.brave = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs;
      [
        (makeDesktopItem {
          name = "brave-private";
          desktopName = "Brave Web Browser (Private)";
          genericName = "Launch a Private Brave-browser Instance";
          icon = "brave";
          exec = "${brave}/bin/brave --incognito";
          categories = [ "Network" ];
        })
      ];

    homeManager.programs.chromium = {
      enable = true;
      package = pkgs.brave;
      extensions = [
        { id = "jhnleheckmknfcgijgkadoemagpecfol"; } # Auto-Tab-Discard
        { id = "nngceckbapebfimnlniiiahkandclblb"; } # Bitwarden
        { id = "eimadpbcbfnmbkopoojfekhnkhdbieeh"; } # Dark-Reader
        { id = "ldpochfccmkkmhdbclfhpagapcfdljkj"; } # Decentraleyes
        { id = "bkdgflcldnnnapblkhphbgpggdiikppg"; } # DuckDuckGo
        { id = "iaiomicjabeggjcfkbimgmglanimpnae"; } # Tab-Session-Manager
        { id = "hipekcciheckooncpjeljhnekcoolahp"; } # Tabliss
        { id = "cjpalhdlnbpafiamejdnhcphjbkeiagm"; } # Ublock-Origin
        { id = "jinjaccalgkegednnccohejagnlnfdag"; } # Violentmonkey
        {
          id = "dcpihecpambacapedldabdbpakmachpb";
          updateUrl =
            "https://raw.githubusercontent.com/iamadamdev/bypass-paywalls-chrome/master/updates.xml";
        }
        (mkIf config.modules.desktop.gnome.enable {
          id = "gphhapmejobijbbhgpjhcjognlahblep"; # Gnome-Shell-Integration
        })
      ];
    };
  };
}
