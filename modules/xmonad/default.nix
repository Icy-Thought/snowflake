{ config, lib, pkgs, ... }:

let

  xmonadPkgs = with pkgs; [
    (rofi.override { plugins = [ rofi-emoji rofi-calc ]; }) # dmenu alt.
    dunst # Notification tool.
    sxiv # Simple X image viewer.
    hsetroot # Compose wallpaper for X.
    redshift # Screen color temp manager.
    stalonetray # Stand alone tray.
    shotgun # Minimal X screenshot util.
    gxmessage # GTK dropin replacement.
  ];

in {
  environment.systemPackages = builtins.concatLists [ xmonadPkgs ];

  services = {
    blueman.enable = true;

    xserver = {
      xkbOptions = "caps:ctrl_modifier";
      displayManager = { defaultSession = "none+xmonad"; };

      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;

        haskellPackages = pkgs.haskellPackages.extend
          (pkgs.haskell.lib.packageSourceOverrides {
            xmonad = pkgs.fetchFromGitHub {
              owner = "xmonad";
              repo = "xmonad";
              rev = "master";
              sha256 = "sq+hDiMfzfYRvzYucpmNt4u60QT9HXH+rJ89jptyMSI=";
            };

            xmonad-contrib = pkgs.fetchFromGitHub {
              owner = "xmonad";
              repo = "xmonad-contrib";
              rev = "master";
              sha256 = "ZX7YU/mp/ORufbL4whnD1vBXVcMqOv8aN+x+lQ7HdOo=";
            };

            xmonad-extras = pkgs.fetchFromGitHub {
              owner = "xmonad";
              repo = "xmonad-extras";
              rev = "master";
              sha256 = "6ikDQVj4Ua8aVTRqugwDYgxltnr7G5KH4cCjdu7vSM4=";
            };
          });

        extraPackages = haskellPackages: [
          haskellPackages.xmonad
          haskellPackages.xmonad-extras
          haskellPackages.xmobar
          haskellPackages.xmonad-contrib
        ];
      };
    };
  };
}
