{ config, pkgs, ... }: {

  programs.emacs = {
    enable = true;
    # package = pkgs.emacsUnstable;
    extraPackages = (epkgs: [ epkgs.vterm ]);
  };

  services.emacs.enable = true;

  home = {
    sessionVariables = {
      DOOMDIR =
        "${config.home.homeDirectory}/git/Snowflake/config/doom-emacs/doom.d";
    };

    file.".doom.d" = {
      source = ./doom.d;
      recursive = true;
      onChange = "doom -y sync -u";
    };
  };

}
