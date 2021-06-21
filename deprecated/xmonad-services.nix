{ config, lib, pkgs, ... }:

let
  xmonadPkgs = with pkgs; [
    rofi                                                            # Menu for X.
    taffybar                                                        # Haskell status-bar.
    nitrogen                                                        # X11 wallpaper browser.
    # xcape                                                           # keymaps modifier.
    # xorg.xkbcomp                                                    # keymaps modifier.
    # xorg.xmodmap                                                    # keymaps modifier.
    # xorg.xrandr                                                     # X Resize and Rotate protocol.
  ];

in {
  environment.systemPackages = builtins.concatLists [
    xmonadPkgs
  ];

  systemd.services = {
    upower.enable = true;
  };

  services = {
    blueman.enable = true;

    xserver = {
      displayManager = {
        defaultSession = "none+xmonad";
      };

      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;

        extraPackages = haskellPackages: [
          haskellPackages.xmonad-contrib
          haskellPackages.xmonad-extras
          haskellPackages.xmonad
        ];
      };

      xkbOptions = "caps:ctrl_modifier";

    };
  };

}
