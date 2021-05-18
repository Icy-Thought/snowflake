{ config, pkgs, ... }:
{
  programs.emacs = {
    enable = true;
    package = with pkgs; [
      emacsUnstable
      emacsWithPackages (epkgs: [ epkgs.vterm ]);
    ]
  };

  # nixpkgs.overlays = [ inputs.emacs.overlays ];
  
  services.emacs = {
    enable = true;
  };
}
