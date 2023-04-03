{ config, lib, pkgs, inputs, ... }:

let
  inherit (lib) attrValues optionalAttrs mkIf mkMerge mkOption;
  inherit (lib.types) str package;
  inherit (lib.my) mkBoolOpt mkOpt;

  cfg = config.modules.desktop.editors.emacs;
  envProto = config.modules.desktop.envProto;
in {
  options.modules.desktop.editors.emacs = {
    package = mkOption {
      type = package;
      default = if (envProto == "wayland") then
        pkgs.emacsPgtk
      else
        pkgs.emacsGit.override { # :FIXME: automate build...
          withGTK3 = true;
          withX = true;
        };
    };
    doomemacs.enable = mkBoolOpt false;
    irkalla.enable = mkBoolOpt false;
  };

  config = mkMerge [
    {
      nixpkgs.overlays = [ inputs.emacs.overlay ];

      hm.services.emacs.enable = true;

      user.packages = attrValues ({
        inherit (pkgs) binutils gnutls zstd;
      } // optionalAttrs config.programs.gnupg.agent.enable {
        inherit (pkgs) pinentry-emacs;
      });

      environment.variables = { EMACSDIR = "$XDG_CONFIG_HOME/emacs"; };

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

        # Content != screen -> eradicated!
        if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
            alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
        fi

        # -------===[ Useful Functions ]===------- #
        emc()      { pgrep emacs && emacsclient -n "$@" || emacs -nw "$@" }
        emc-diff() { emacs -nw --eval "(ediff-files \"$1\" \"$2\")"; }
        emc-man()  { emacs -nw --eval "(switch-to-buffer (man \"$1\"))"; }
        emc-kill() { emacsclient --eval '(kill-emacs)'; }
      '';

      hm.programs.fish = {
        # Easier frame creation (fish)
        functions = {
          eg = "emacs --create-frame $argv & disown";
          ecg = "emacsclient --create-frame $argv & disown";
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

    (mkIf cfg.irkalla.enable {
      hm.programs.emacs = {
        enable = true;
        package = cfg.package;
        extraPackages = epkgs: with epkgs; [ pdf-tools vterm ];
      };

      home.configFile = {
        irkalla-init = {
          target = "emacs/init.el";
          source = "${inputs.emacs-dir}/irkalla/init.el";
        };
        irkalla-early-init = {
          target = "emacs/early-init.el";
          source = "${inputs.emacs-dir}/irkalla/early-init.el";
        };
        irkalla-dasHead = {
          target = "emacs/dasHead.svg";
          source = "${inputs.emacs-dir}/irkalla/dasHead.svg";
        };
      };
    })

    (mkIf cfg.doomemacs.enable {
      hm.imports = [ inputs.doomemacs.hmModule ];

      hm.programs.doom-emacs = {
        enable = true;
        emacsPackage = cfg.package;
        doomPrivateDir = "${inputs.emacs-dir}/doom-config";
      };

      env.PATH = [ "$XDG_CONFIG_HOME/emacs/bin" ];

      environment.variables = {
        DOOMDIR = "$XDG_CONFIG_HOME/doomemacs";
        DOOMLOCALDIR = "$XDG_DATA_HOME/doomemacs";
      };
    })
  ];
}
