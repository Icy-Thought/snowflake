{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.browsers.brave;
in {
  options.modules.desktop.browsers.brave = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      (makeDesktopItem {
        name = "brave-private";
        desktopName = "Brave Web Browser (Private)";
        genericName = "Launch a Private Brave-browser Instance";
        icon = "brave";
        exec = "${getExe brave} --incognito";
        categories = ["Network"];
      })
    ];

    hm.programs.chromium = {
      enable = true;
      package = pkgs.brave;
      extensions = [
        {id = "jhnleheckmknfcgijgkadoemagpecfol";} # Auto-Tab-Discard
        {id = "nngceckbapebfimnlniiiahkandclblb";} # Bitwarden
        {id = "dlnejlppicbjfcfcedcflplfjajinajd";} # Bonjourr (New-Tab Page)
        {id = "eimadpbcbfnmbkopoojfekhnkhdbieeh";} # Dark-Reader
        {id = "ldpochfccmkkmhdbclfhpagapcfdljkj";} # Decentraleyes
        {id = "bkdgflcldnnnapblkhphbgpggdiikppg";} # DuckDuckGo
        {id = "hlepfoohegkhhmjieoechaddaejaokhf";} # Refined GitHub
        {id = "iaiomicjabeggjcfkbimgmglanimpnae";} # Tab-Session-Manager
        {id = "cjpalhdlnbpafiamejdnhcphjbkeiagm";} # Ublock-Origin
        {id = "jinjaccalgkegednnccohejagnlnfdag";} # Violentmonkey
        {
          id = "dcpihecpambacapedldabdbpakmachpb";
          updateUrl = "https://raw.githubusercontent.com/iamadamdev/bypass-paywalls-chrome/master/src/updates/updates.xml";
        }
        (mkIf config.modules.desktop.gnome.enable [
          {id = "gphhapmejobijbbhgpjhcjognlahblep";} # Gnome-Shell-Integration
        ])
      ];
    };
  };
}
