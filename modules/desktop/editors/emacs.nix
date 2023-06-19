{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  inherit (lib.attrsets) attrValues optionalAttrs;
  inherit (lib.modules) mkIf mkMerge;

  cfg = config.modules.desktop.editors.emacs;
  envProto = config.modules.desktop.envProto;
in {
  options.modules.desktop.editors.emacs = let
    inherit (lib.options) mkEnableOption mkOption mkPackageOption;
    inherit (lib.types) enum nullOr package;
  in {
    enable = mkEnableOption "Spread the joy of Emacs in our flake";
    package = mkPackageOption pkgs "emacs" {
      default =
        if (envProto == "wayland")
        then "emacs-pgtk"
        else "emacs-git";
    };
    transparency = {
      enable = mkEnableOption "Appropriate transparency for our Emacs frame";
      package = mkOption {
        type = package;
        default =
          if cfg.transparency.enable
          then
            cfg.package.override {
              withGTK3 = true;
              withX = true;
            }
          else cfg.package;
        description = "(temporary) non-sensical Emacs solution..";
      };
    };
    template = mkOption {
      type = nullOr (enum ["doomemacs" "irkalla"]);
      default = "irkalla";
      description = "Which Emacs configuration to setup.";
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      nixpkgs.overlays = [inputs.emacs.overlay];

      hm.services.emacs.enable = true;

      user.packages = attrValues ({
          inherit (pkgs) binutils gnutls zstd;
        }
        // optionalAttrs config.programs.gnupg.agent.enable {
          inherit (pkgs) pinentry-emacs;
        });

      environment.variables = {EMACSDIR = "$XDG_CONFIG_HOME/emacs";};

      hm.programs.zsh.initExtra = ''
        # -------===[ VTerm Integration ]===------- #
        function vterm_printf(){
           if [ -n "$TMUX" ] && ([ "''${TERM%%-*}" = "tmux" ] || [ "''${TERM%%-*}" = "screen" ] ); then
               # Tell tmux to pass the escape sequences through
               printf "\ePtmux;\e\e]%s\007\e\\" "$1"
           elif [ "''${TERM%%-*}" = "screen" ]; then
               # GNU screen (screen, screen-256color, screen-256color-bce)
               printf "\eP\e]%s\007\e\\" "$1"
           else
               printf "\e]%s\e\\" "$1"
           fi
        }

        # -------===[ Useful Functions ]===------- #
        emc()      { pgrep emacs && emacsclient -n "$@" || emacs -nw "$@" }
        emc-diff() { emacs -nw --eval "(ediff-files \"$1\" \"$2\")"; }
        emc-kill() { emacsclient --eval '(kill-emacs)'; }
        emc-man()  { emacs -nw --eval "(switch-to-buffer (man \"$1\"))"; }
      '';

      hm.programs.fish = {
        # Easier frame creation (fish)
        functions = {
          emc = "pgrep emacs && emacsclient -n $argv || emacs -nw $argv";
          emc-diff = "emacs -nw --eval \"(ediff-files '$argv[1]' '$argv[2]')\"";
          emc-kill = "emacsclient --eval '(kill-emacs)'";
          emc-man = "emacs -nw --eval \"(switch-to-buffer (man '$argv[1]'))\"";
        };

        # Allow fish-shell to send information to vterm via properly escaped sequences.
        interactiveShellInit = ''
          function vterm_printf;
              if begin; [  -n "$TMUX" ]  ; and  string match -q -r "screen|tmux" "$TERM"; end
                  # tell tmux to pass the escape sequences through
                  printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
              else if string match -q -- "screen*" "$TERM"
                  # GNU screen (screen, screen-256color, screen-256color-bce)
                  printf "\eP\e]%s\007\e\\" "$argv"
              else
                  printf "\e]%s\e\\" "$argv"
              end
          end
        '';
      };

      programs.xonsh.config = ""; # TODO
    }

    (mkIf (cfg.template == "irkalla") {
      hm.programs.emacs = {
        enable = true;
        package = cfg.transparency.package;
        extraPackages = epkgs:
          attrValues {
            inherit (epkgs) jinx pdf-tools vterm;
            treesitter-grammars = epkgs.treesit-grammars.with-grammars (grammars:
              with grammars; [
                tree-sitter-bash
                tree-sitter-elisp
                tree-sitter-nix
                tree-sitter-json
                tree-sitter-fish
                tree-sitter-latex
                # tree-sitter-ledger
                tree-sitter-haskell
                tree-sitter-markdown-inline
                tree-sitter-rust
                tree-sitter-toml
                tree-sitter-typescript
                tree-sitter-yaml
              ]);
          };
      };

      home.configFile = {
        irkalla-dasHead = {
          target = "emacs/dasHead.svg";
          source = "${inputs.emacs-dir}/irkalla/dasHead.svg";
        };
        irkalla-init = {
          source = "${inputs.emacs-dir}/irkalla/config.org";
          target = "emacs/config.org";
          onChange = let
            initFiles = "${config.hm.xdg.configHome}/emacs/config.org";
          in ''
            ${cfg.transparency.package}/bin/emacs --batch \
              --eval "(require 'ob-tangle)" \
              --eval "(setq org-confirm-babel-evaluate nil)" \
              --eval '(org-babel-tangle-file "${initFiles}")'
          '';
        };
      };
    })

    (mkIf (cfg.template == "doomemacs") {
      hm.imports = [inputs.doomemacs.hmModule];

      hm.programs.doom-emacs = {
        enable = true;
        emacsPackage = cfg.transparency.package;
        doomPrivateDir = "${inputs.emacs-dir}/doom-config";
      };

      env.PATH = ["$XDG_CONFIG_HOME/emacs/bin"];

      environment.variables = {
        DOOMDIR = "$XDG_CONFIG_HOME/doomemacs";
        DOOMLOCALDIR = "$XDG_DATA_HOME/doomemacs";
      };
    })
  ]);
}
