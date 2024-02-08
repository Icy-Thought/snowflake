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
    enable = mkEnableOption "Sprinkle a bit of magic to our nix-flake.";
    package = mkOption {
      type = package;
      default = let
        inherit (pkgs) emacs29-gtk3 emacs29-pgtk;
      in
        if (envProto == "wayland")
        then emacs29-pgtk
        else emacs29-gtk3;
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
      user.packages = attrValues ({
          inherit (pkgs) binutils gnutls zstd notmuch;
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
          attrValues {
            inherit (epkgs.melpaPackages) jinx pdf-tools telega vterm;
            inherit (epkgs.treesit-grammars) with-all-grammars;
          };
      };

      hm.services.emacs = {
        enable = true;
        startWithUserSession = true;
      };

      hm.programs.zsh.initExtra = ''
        # -------===[ EAT Integration ]===------- #
        [ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
          source "$EAT_SHELL_INTEGRATION_DIR/zsh"

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

        interactiveShellInit = ''
        ''; # EAT lacks fish integration
      };

      programs.xonsh.config = ""; # TODO
    }

    (mkIf (cfg.template == "irkalla") {
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
      home.configFile.doomemacs-conf = {
        target = "doomemacs";
        source = "${inputs.emacs-dir}/doomemacs";
        recursive = true;
        onChange = "doom -y sync -u";
      };

      env.PATH = ["$XDG_CONFIG_HOME/emacs/bin"];

      environment.variables = {
        DOOMDIR = "$XDG_CONFIG_HOME/doomemacs";
        DOOMLOCALDIR = "$XDG_DATA_HOME/doomemacs";
      };
    })
  ]);
}
