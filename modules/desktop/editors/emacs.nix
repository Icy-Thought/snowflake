{ config, lib, pkgs, inputs, ... }:

let cfg = config.modules.desktop.editors.emacs;
in with lib; {
  options.modules.desktop.editors.emacs = {
    enable = mkEnableOption "Sprinkle a bit of magic to the nix-flake.";
    package = mkOption {
      description = "Emacs version to be installed.";
      type = types.package;
      default = if (config.modules.desktop.type == "wayland") then
        pkgs.emacs30-pgtk
      else
        pkgs.emacs30-gtk3;
    };
    terminal = mkOption {
      description = "Terminal emulator to be installed in Emacs.";
      type = types.nullOr (types.enum [ "Eat" "VTerm" ]);
      default = "VTerm";
    };
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs;
      [ binutils gnutls zstd unstable.emacs-lsp-booster my.my-cookies ]
      ++ optionals config.programs.gnupg.agent.enable [ pinentry-emacs ];
    environment.wordlist.enable = true; # cape-dict

    hm.programs.emacs = {
      enable = true;
      package = cfg.package;
      extraPackages = epkgs:
        with epkgs;
        [
          mu4e
          melpaPackages.jinx
          melpaPackages.pdf-tools
          treesit-grammars.with-all-grammars
          (melpaPackages.telega.overrideAttrs (_: {
            version = "0.8.290";
            src = pkgs.sources.telega;
          }))
        ] ++ optionals (cfg.terminal == "VTerm") [ melpaPackages.vterm ];
    };

    hm.services.emacs = {
      enable = true;
      client = {
        enable = true;
        arguments = [ "--create-frame" "--no-wait" ];
      };
      socketActivation.enable = true;
    };

    hm.programs.zsh.initExtra = ''
      # -------===[ Useful Functions ]===------- #
      ediff()  { emacsclient -c -a \'\' --eval "(ediff-files \"$1\" \"$2\")"; }
      edired() { emacsclient -c -a \'\' --eval "(progn (dired \"$1\"))"; }
      ekill()  { emacsclient -c -a \'\' --eval '(kill-emacs)'; }
      eman()   { emacsclient -c -a \'\' --eval "(switch-to-buffer (man \"$1\"))"; }
      magit()  { emacsclient -c -a \'\' --eval '(magit-status)'; }
    '' + optionalString (cfg.terminal == "Eat") ''
      # -------===[ EAT Integration ]===------- #
      [ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
        source "$EAT_SHELL_INTEGRATION_DIR/zsh"
    '' + optionalString (cfg.terminal == "VTerm") ''
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
        ediff =
          "emacsclient -c -a '' --eval \"(ediff-files '$argv[1]' '$argv[2]')\"";
        edired = "emacsclient -c -a '' --eval \"(progn (dired '$argv[1]'))\"";
        ekill = "emacsclient -c -a '' --eval '(kill-emacs)'";
        eman =
          "emacsclient -c -a '' --eval \"(switch-to-buffer (man '$argv[1]'))\"";
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

    create.configFile.emacs-init =
      let configFile = "${inputs.emacs-dir}/config.org";
      in {
        target = "emacs/config.org";
        source = "${configFile}";
        onChange = ''
          ${getExe cfg.package} -Q --batch \
          -l ob-tangle "${configFile}" -f org-babel-tangle
        '';
      };
  };
}
