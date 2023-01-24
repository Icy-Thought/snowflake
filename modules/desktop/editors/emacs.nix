{ config
, lib
, pkgs
, inputs
, ...
}:

let
  inherit (lib) mkIf mkMerge mkOption;
  inherit (lib.types) str package;
  inherit (lib.my) mkBoolOpt mkOpt;

  cfg = config.modules.desktop.editors.emacs;
  envProto = config.modules.desktop.envProto;
in
{
  options.modules.desktop.editors.emacs = {
    package = mkOption {
      type = package;
      default =
        if (envProto == "wayland") then pkgs.emacsPgtk
        else pkgs.emacsGit;
    };
    doomemacs = rec {
      enable = mkBoolOpt false;
      forgeUrl = mkOpt str "https://github.com";
      repoUrl = mkOpt str "${forgeUrl}/doomemacs/doomemacs";
      configRepoUrl = mkOpt str "${forgeUrl}/icy-thought/emacs/";
    };
    irkalla.enable = mkBoolOpt false;
  };

  config = mkMerge [
    {
      nixpkgs.overlays = [ inputs.emacs.overlay ];

      hm.programs.emacs = {
        enable = true;
        package = cfg.package;
        extraPackages = epkgs: with epkgs; [ pdf-tools vterm ];
      };

      user.packages = with pkgs; [
        binutils
        gnutls
        zstd
        (mkIf (config.programs.gnupg.agent.enable) pinentry-emacs)
      ];

      environment.variables = {
        EMACSDIR = "$XDG_CONFIG_HOME/emacs";
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

      programs.xonsh.config = ''
      ''; # TODO
    }

    (mkIf cfg.irkalla.enable {
      home.configFile.irkalla-conf = {
        target = "emacs";
        source = "${inputs.emacs-dir}/irkalla";
        recursive = true;
      };
    })

    (mkIf cfg.doomemacs.enable {
      env.PATH = [ "$XDG_CONFIG_HOME/emacs/bin" ];

      environment.variables = {
        DOOMDIR = "$XDG_CONFIG_HOME/doomemacs";
        DOOMLOCALDIR = "$XDG_DATA_HOME/doomemacs";
      };

      home.configFile.doom-conf = {
        target = "doomemacs";
        source = "${inputs.emacs-dir}/doom-config";
        recursive = true;
      };

      # TODO: resort to manual solution for now...
      # home.activation = {
      #   installDoomEmacs = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      #     if [ ! -d "$XDG_CONFIG_HOME/emacs" ]; then
      #        git clone --depth=1 --single-branch "${cfg.doomemacs.repoUrl}" "$XDG_CONFIG_HOME/emacs"
      #        git clone "${cfg.doomemacs.configRepoUrl}" "$XDG_CONFIG_HOME/doomemacs"
      #     fi
      #   '';
      # };
    })
  ];
}
