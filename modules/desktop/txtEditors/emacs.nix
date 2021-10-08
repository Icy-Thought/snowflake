{ config, lib, pkgs, inputs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.editors.emacs;
  configDir = config.snowflake.configDir;
in {
  options.modules.editors.emacs = {
    enable = mkBoolOpt false;
    doom = {
      enable = mkBoolOpt true;
      fromSSH = mkBoolOpt false;
    };
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays = [ inputs.emacs-overlay.overlay ];

    user.packages = with pkgs; [
      binutils
      ((emacsPackagesNgGen emacsPgtkGcc).emacsWithPackages
        (epkgs: [ epkgs.vterm ]))

      ## Doom Dependencies
      (ripgrep.override { withPCRE2 = true; })
      gnutls

      # Extra Dependencies:
      fd
      imagemagick
      (mkIf (config.programs.gnupg.agent.enable) pinentry_emacs)
      zstd
      # :checkers spell
      (aspellWithDicts (ds: with ds; [ en en-computers en-science ]))
      # :checkers grammar
      languagetool
      # :tools editorconfig
      editorconfig-core-c
      # :tools lookup & :lang org (roam + gnuplot + latex)
      sqlite
      graphviz
      gnuplot
      # :lang cc
      ccls
      # :lang haskell
      stylish-haskell
      haskell-language-server
      # :lang javascript
      nodePackages.javascript-typescript-langserver
      # :lang latex & :lang org (latex previews)
      texlive.combined.scheme-medium
      # :lang nix
      nixfmt
      # :lang rust
      rust-analyzer
    ];

    fonts.fonts = [ pkgs.emacs-all-the-icons-fonts ];

    services.emacs.enable = true;

    env.PATH = [ "$XDG_CONFIG_HOME/emacs/bin" ];

    home.sessionVariables.DOOMDIR =
      "${config.home.homeDirectory}/git/Icy-Thought/Snowflake/config/doom-emacs/config";

    home.file.".doom.d" = {
      source = "${configDir}/doom-emacs";
      recursive = true;
      onChange = "doom -y sync -u";
    };

    # init.doomEmacs = mkIf cfg.doom.enable ''
    #   if [ -d $HOME/.config/emacs ]; then
    #      ${optionalString cfg.doom.fromSSH ''
    #         git clone git@github.com:hlissner/doom-emacs.git $HOME/.config/emacs
    #      ''}
    #      ${optionalString (cfg.doom.fromSSH == false) ''
    #         git clone https://github.com/hlissner/doom-emacs $HOME/.config/emacs
    #      ''}
    #   fi
    # '';
  };
}
