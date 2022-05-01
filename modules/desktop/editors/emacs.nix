{ config, lib, pkgs, inputs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.editors.emacs;
  configDir = config.snowflake.configDir;
in {
  options.modules.desktop.editors.emacs = {
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
      package = pkgs.emacsNativeComp;
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

    # system.userActivationScripts = mkIf cfg.doom.enable {
    #   installDoomEmacs = ''
    #     if [ ! -d "$HOME/.config/emacs" ]; then
    #        git clone --depth=1 --single-branch https://github.com/hlissner/doom-emacs "$XDG_CONFIG_HOME/emacs"
    #     fi
    #   '';
    # };
  };
}
