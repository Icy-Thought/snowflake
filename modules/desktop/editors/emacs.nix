{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  inherit (lib.attrsets) attrValues optionalAttrs;
  inherit (lib.modules) mkIf mkMerge;
  inherit (lib.meta) getExe;
  inherit (lib.strings) optionalString;
  cfg = config.modules.desktop.editors.emacs;
in {
  options.modules.desktop.editors.emacs = let
    inherit (lib.options) mkEnableOption mkOption;
    inherit (lib.types) enum nullOr package;
  in {
    enable = mkEnableOption "Sprinkle a bit of magic to our nix-flake.";
    package = mkOption {
      type = package;
      default =
        if (config.modules.desktop.type == "wayland")
        then pkgs.emacs-pgtk
        else pkgs.emacs-git.override {withGTK3 = true;};
      description = "Emacs package which will be installed in our flake system.";
    };
    terminal = mkOption {
      type = nullOr (enum ["Eat" "VTerm"]);
      default = "VTerm";
      description = "Terminal emulator used within Emacs.";
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
          inherit (pkgs.unstable) emacs-lsp-booster;
          inherit (pkgs.my) my-cookies; # leetcode.el
        }
        // optionalAttrs config.programs.gnupg.agent.enable {
          inherit (pkgs) pinentry-emacs;
        });
      environment.wordlist.enable = true; # cape-dict

      hm.programs.emacs = {
        enable = true;
        package = cfg.package;
        extraPackages = epkgs:
          attrValues ({
              inherit (epkgs.melpaPackages) jinx pdf-tools;
              inherit (epkgs.treesit-grammars) with-all-grammars;
              telega = epkgs.melpaPackages.telega.overrideAttrs (_: {
                version = "0.8.290";
                src = pkgs.sources.telega;
              });
            }
            // optionalAttrs (cfg.terminal == "VTerm") {
              inherit (epkgs.melpaPackages) vterm;
            });
      };

      hm.services.emacs = {
        enable = true;
        client = {
          enable = true;
          arguments = ["--create-frame" "--no-wait"];
        };
        socketActivation.enable = true;
      };

      hm.programs.zsh.initExtra =
        ''
          # -------===[ Useful Functions ]===------- #
          ediff()  { emacsclient -c -a \'\' --eval "(ediff-files \"$1\" \"$2\")"; }
          edired() { emacsclient -c -a \'\' --eval "(progn (dired \"$1\"))"; }
          ekill()  { emacsclient -c -a \'\' --eval '(kill-emacs)'; }
          eman()   { emacsclient -c -a \'\' --eval "(switch-to-buffer (man \"$1\"))"; }
          magit()  { emacsclient -c -a \'\' --eval '(magit-status)'; }
        ''
        + optionalString (cfg.terminal == "Eat") ''
          # -------===[ EAT Integration ]===------- #
          [ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
            source "$EAT_SHELL_INTEGRATION_DIR/zsh"
        ''
        + optionalString (cfg.terminal == "VTerm") ''
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
        '';

      hm.programs.fish = {
        functions = {
          ediff = "emacsclient -c -a '' --eval \"(ediff-files '$argv[1]' '$argv[2]')\"";
          edired = "emacsclient -c -a '' --eval \"(progn (dired '$argv[1]'))\"";
          ekill = "emacsclient -c -a '' --eval '(kill-emacs)'";
          eman = "emacsclient -c -a '' --eval \"(switch-to-buffer (man '$argv[1]'))\"";
          magit = " emacsclient -c -a '' --eval '(magit-status)'";
        };

        interactiveShellInit = optionalString (cfg.terminal == "VTerm") ''
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
      create.configFile = {
        irkalla-init = let
          configFile = "${inputs.emacs-dir}/config.org";
        in {
          target = "emacs/init.org";
          source = "${configFile}";
          onChange = ''
            ${getExe cfg.package} --batch \
              --eval "(require 'ob-tangle)" \
              --eval "(setq org-confirm-babel-evaluate nil)" \
              --eval '(org-babel-tangle-file "${configFile}")'
          '';
        };
      };
    })

    (mkIf (cfg.template == "doomemacs") {
      home = {
        sessionPath = ["$XDG_CONFIG_HOME/emacs/bin"];
        variables = {
          DOOMDIR = "$XDG_CONFIG_HOME/doomemacs";
          DOOMLOCALDIR = "$XDG_DATA_HOME/doomemacs";
        };
        configFile.doomemacs-conf = {
          target = "doomemacs";
          source = "${inputs.emacs-dir}";
          recursive = true;
          onChange = "doom -y sync -u";
        };
      };
    })
  ]);
}
