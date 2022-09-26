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
        genericName = "Launch a Private Ungoogled-Chromium Instance";
        icon = "brave";
        exec = "${getExe ungoogled-chromium} --incognito";
        categories = [ "Network" ];
      })
    ];

    hm.programs.chromium = {
      enable = true;
      package = pkgs.ungoogled-chromium;
      extensions = [
        {
          id = "ocaahdebbfolfmndjeplogmgcagdmblk";
          updateUrl = "https://raw.githubusercontent.com/NeverDecaf/chromium-web-store/master/updates.xml";
        }
        { id = "jhnleheckmknfcgijgkadoemagpecfol"; } # Auto-Tab-Discard
        { id = "nngceckbapebfimnlniiiahkandclblb"; } # Bitwarden
        { id = "eimadpbcbfnmbkopoojfekhnkhdbieeh"; } # Dark-Reader
        { id = "ldpochfccmkkmhdbclfhpagapcfdljkj"; } # Decentraleyes
        { id = "bkdgflcldnnnapblkhphbgpggdiikppg"; } # DuckDuckGo
        { id = "gphhapmejobijbbhgpjhcjognlahblep"; } # Gnome-Shell-Integration
        { id = "iaiomicjabeggjcfkbimgmglanimpnae"; } # Tab-Session-Manager
        { id = "hipekcciheckooncpjeljhnekcoolahp"; } # Tabliss
        { id = "cjpalhdlnbpafiamejdnhcphjbkeiagm"; } # Ublock-Origin
        {
          id = "dcpihecpambacapedldabdbpakmachpb";
          updateUrl = "https://raw.githubusercontent.com/iamadamdev/bypass-paywalls-chrome/master/updates.xml";
        }
      ];
    };
  };
}
