{ config, lib, pkgs, inputs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.appliances.editors.emacs;
  configDir = config.snowflake.configDir;
in {
  options.modules.appliances.editors.emacs = {
    enable = mkBoolOpt false;
    doom = {
      enable = mkBoolOpt true;
      fromSSH = mkBoolOpt false;
    };
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays = [ inputs.emacs.overlay ];

    user.packages = with pkgs; [
      # Emacs Dependencies:
      binutils
      ((emacsPackagesNgGen emacsPgtkGcc).emacsWithPackages
        (epkgs: with epkgs; [ vterm pdf-tools emojify ]))

      # Doom Dependencies
      (ripgrep.override { withPCRE2 = true; })
      gnutls

      # Extra Dependencies:
      fd
      imagemagick
      (mkIf (config.programs.gnupg.agent.enable) pinentry_emacs)
      zstd

      # Module dependencies:
      ## :checkers spell
      (aspellWithDicts (ds: with ds; [ en en-computers en-science ]))
      ## :tools editorconfig
      editorconfig-core-c
      ## :tools lookup & :lang Org-Roam & Gnuplot
      sqlite
      gnuplot
      ## :lang haskell
      haskell-language-server
      stylish-haskell
      ## :lang javascript
      nodePackages.typescript-language-server
      ## :lang latex & :lang org (latex previews)
      texlive.combined.scheme-medium
      ## :lang nix
      nixfmt
      rnix-lsp
      ## :lang rust
      unstable.rust-analyzer
    ];

    # Fonts -> icons + ligatures when specified:
    fonts.fonts = [ pkgs.emacs-all-the-icons-fonts ];

    # Enable access to doom (tool).
    env.PATH = [ "$XDG_CONFIG_HOME/emacs/bin" ];

    # Default DOOMDIR => Snowflake/config/doom-emacs.
    environment.variables = {
      EMACSDIR = "$XDG_CONFIG_HOME/emacs";
      DOOMDIR = "${configDir}/doom-emacs";
    };

    # init.doomEmacs = mkIf cfg.doom.enable ''
    #   if [ -d $HOME/.config/emacs ]; then
    #      ${optionalString cfg.doom.fromSSH ''
    #         git clone git@github.com:hlissner/doom-emacs.git $HOME/.config/emacs
    #      ''}
    #
    #      ${optionalString (cfg.doom.fromSSH == false) ''
    #         git clone https://github.com/hlissner/doom-emacs $HOME/.config/emacs
    #      ''}
    #   fi
    # '';
  };
}
