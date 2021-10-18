{ config, lib, pkgs, inputs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.txtEditor.emacs;
  configDir = config.snowflake.configDir;

  emacsWithPackages = with pkgs;
    ((emacsPackagesNgGen emacsPgtkGcc).emacsWithPackages
      (epkgs: [ epkgs.vterm epkgs.pdf-tools epkgs.emojify ]));
in {
  options.modules.desktop.txtEditor.emacs = {
    enable = mkBoolOpt false;
    doom = {
      enable = mkBoolOpt true;
      fromSSH = mkBoolOpt false;
    };
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays = [ inputs.emacs.overlay ];

    user.packages = with pkgs; [
      binutils
      emacsWithPackages

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
      # :lang javascript
      nodePackages.javascript-typescript-langserver
      # :lang latex & :lang org (latex previews)
      texlive.combined.scheme-medium
      # :lang nix
      nixfmt
      rnix-lsp
      # :lang rust
      rust-analyzer
    ];

    fonts.fonts = [ pkgs.emacs-all-the-icons-fonts ];

    # Enable access to doom (tool).
    env.PATH = [ "$XDG_CONFIG_HOME/emacs/bin" ];

    # Default DOOMDIR => Snowflake/config/doom-emacs.
    environment.variables = {
      EMACSDIR = "$XDG_CONFIG_HOME/emacs";
      DOOMDIR = "${configDir}/doom-emacs";
    };

    # Enable emacs --daemon on envManager launch.
    services.emacs = {
      enable = true;
      package = emacsWithPackages;
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
