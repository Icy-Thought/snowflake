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
    inherit (lib.options) mkEnableOption mkOption;
    inherit (lib.types) enum nullOr package;
  in {
    enable = mkEnableOption "Spread the joy of Emacs in our flake";
    package = mkOption {
      type = package;
      default = let
        inherit (pkgs) emacs-git emacs-pgtk;
      in
        if (envProto == "wayland")
        then emacs-pgtk
        else emacs-git.override {toolkit = "no";};
      description = "Emacs package which will be installed in our flake system.";
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

      user.packages = attrValues ({
          inherit (pkgs) binutils gnutls zstd;
        }
        // optionalAttrs config.programs.gnupg.agent.enable {
          inherit (pkgs) pinentry-emacs;
        });

      hm.services.emacs = {
        enable = true;
        startWithUserSession = true;
      };

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
        ediff()  { emacsclient -c -a \'\' --eval "(ediff-files \"$1\" \"$2\")"; }
        edired() { emacsclient -c -a \'\' --eval "(progn (dired \"$1\"))"; }
        ekill()  { emacsclient -c -a \'\' --eval '(kill-emacs)'; }
        eman()   { emacsclient -c -a \'\' --eval "(switch-to-buffer (man \"$1\"))"; }
        magit()  { emacsclient -c -a \'\' --eval '(magit-status)'; }
      '';

      hm.programs.fish = {
        functions = {
          ediff = "emacsclient -c -a '' --eval \"(ediff-files '$argv[1]' '$argv[2]')\"";
          edired = "emacsclient -c -a '' --eval \"(progn (dired '$argv[1]'))\"";
          ekill = "emacsclient -c -a '' --eval '(kill-emacs)'";
          eman = "emacsclient -c -a '' --eval \"(switch-to-buffer (man '$argv[1]'))\"";
          magit = " emacsclient -c -a '' --eval '(magit-status)'";
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
        package = cfg.package;
        extraPackages = epkgs:
          attrValues {
            inherit (epkgs.melpaPackages) jinx pdf-tools telega vterm;
            inherit (epkgs.treesit-grammars) with-all-grammars;
          };
      };

      home.configFile = {
        irkalla-early-init = {
          target = "emacs/early-init.el";
          source = "${inputs.emacs-dir}/irkalla/early-init.el";
        };
        irkalla-init = {
          target = "emacs/init.el";
          source = "${inputs.emacs-dir}/irkalla/init.el";
        };
      };
    })

    (mkIf (cfg.template == "doomemacs") {
      hm.imports = [inputs.doomemacs.hmModule];

      hm.programs.doomemacs = {
        enable = true;
        emacsPackage = cfg.package;
        doomPrivateDir = "${inputs.emacs-dir}/doomconfig";
      };

      env.PATH = ["$XDG_CONFIG_HOME/emacs/bin"];

      environment.variables = {
        DOOMDIR = "$XDG_CONFIG_HOME/doomemacs";
        DOOMLOCALDIR = "$XDG_DATA_HOME/doomemacs";
      };
    })
  ]);
}
