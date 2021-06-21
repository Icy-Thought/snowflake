{ config, pkgs, ... }:
{
  programs.emacs = {
    enable = true;
    # package = pkgs.emacsUnstable;
    extraPackages = (epkgs: [ epkgs.vterm ] );
  };

  services.emacs = {
    enable = true;
  };
}
