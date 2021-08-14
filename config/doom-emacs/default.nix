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
        "${config.home.homeDirectory}/git/Icy-Thought/Snowflake/config/doom-emacs/config";
    };

    file.".doom.d" = {
      source = ./config;
      recursive = true;
      onChange = "doom -y sync -u";
    };
  };

}
