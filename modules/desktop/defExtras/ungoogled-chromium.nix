{ config, lib, pkgs, ... }:

{
  programs.chromium = {
    enable = true;
    package = pkgs.ungoogled-chromium;
    extensions = [
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
        updateUrl =
          "https://raw.githubusercontent.com/iamadamdev/bypass-paywalls-chrome/master/updates.xml";
      }
    ];
  };
}
