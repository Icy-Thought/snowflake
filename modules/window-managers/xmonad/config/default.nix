{ config, lib, pkgs, ... }:

{
  imports = [ ./dunst ./rofi ./xsession ./scripts ./xresources ];

  config.xsession.windowManager.command =
    "${pkgs.haskellPackages.icy-xmonad}/bin/icy-xmonad";

}
