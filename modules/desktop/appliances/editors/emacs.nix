{ config, lib, pkgs, inputs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.appliances.editors.emacs;
  configDir = config.snowflake.configDir;
in {
  options.modules.desktop.appliances.editors.emacs = {
    enable = mkBoolOpt false;
    doom = {
      enable = mkBoolOpt true;
      fromSSH = mkBoolOpt false;
    };
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays = [ inputs.emacs.overlay ];

    homeManager.services.emacs = {
      enable = true;
      client.enable = true;
    };

    homeManager.programs.emacs = {
      enable = true;
      package = pkgs.emacsGcc;
      extraPackages = epkgs: with epkgs; [ vterm ];
    };

    user.packages = with pkgs; [
      # emacsPkgs:
      binutils
      # doomPkgs
      gnutls
      # extraPkgs
      zstd
      (mkIf (config.programs.gnupg.agent.enable) pinentry_emacs)
      # Module dependencies
      ## :tools lookup
      sqlite
    ];

    # Fonts -> icons + ligatures when specified:
    fonts.fonts = [ pkgs.emacs-all-the-icons-fonts ];

    # Enable access to doom (tool).
    env.PATH = [ "$XDG_CONFIG_HOME/emacs/bin" ];

    environment.variables = {
      EMACSDIR = "$XDG_CONFIG_HOME/emacs";
      DOOMDIR = "${configDir}/emacs.d/doom-emacs";
    };

    # init.doomEmacs = mkIf cfg.doom.enable ''
    #   if [ -d $HOME/.config/emacs ]; then
    #      ${optionalString cfg.doom.fromSSH ''
    #         git clone --depth 1 git@github.com:hlissner/doom-emacs.git $HOME/.config/emacs
    #      ''}
    #      ${optionalString (cfg.doom.fromSSH == false) ''
    #         git clone --depth 1 https://github.com/hlissner/doom-emacs $HOME/.config/emacs
    #      ''}
    #   fi
    # '';
  };
}
