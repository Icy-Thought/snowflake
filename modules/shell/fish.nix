{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.fish;
in {
  options.modules.shell.fish = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    users.defaultUserShell = pkgs.fish;

    user.packages = with pkgs; [
      any-nix-shell
      bat
      exa
      fd
      fzf
      pipes-rs
      pwgen
      ripgrep
      tldr
      youtube-dl
    ];

    homeManager.programs.fish = {
      enable = true;
      plugins = with config.modules.shell; [
        {
          name = "fzf";
          src = pkgs.fetchFromGitHub {
            owner = "PatrickF1";
            repo = "fzf.fish";
            rev = "628f04bf239bc6b820c090b8c814ca3c242142d7";
            sha256 = "EiNaEhqJbisv/gA5th2IWh4XIQ3zlGrXTpMMTaUb0Ag=";
          };
        }

        (mkIf git.enable {
          name = "git";
          src = pkgs.fetchFromGitHub {
            owner = "jhillyerd";
            repo = "plugin-git";
            rev = "1a0357c1f13a9c5f18a5d773e9c0c963f1ff23b6";
            sha256 = "d1Mcn+u9aLI3PLDXth0VyoqZiO6Z6R/yL7f/RwIR0/o=";
          };
        })
      ];

      shellInit = ''
        # General Configurations
        set fish_greeting
        set -gx EDITOR nvim
        set -g fish_key_bindings fish_vi_key_bindings

        # Customizable fish_title
        function fish_title
            echo $argv[1]
        end

        # Tmux on terminal start
        if status is-interactive
        and not set -q TMUX
            exec tmux
        end

        # Colored man-pages
        set -xU LESS_TERMCAP_md (printf "\e[01;31m")
        set -xU LESS_TERMCAP_me (printf "\e[0m")
        set -xU LESS_TERMCAP_se (printf "\e[0m")
        set -xU LESS_TERMCAP_so (printf "\e[01;44;33m")
        set -xU LESS_TERMCAP_ue (printf "\e[0m")
        set -xU LESS_TERMCAP_us (printf "\e[01;32m")

        # Emacs: Vterm
        # Allow shell to send information to vterm via properly escaped sequences.
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

        # Sources
        starship init fish | source
      '';

      shellAliases = { exa = "exa --group-directories-first"; };

      shellAbbrs = {
        # General
        ls = "exa -Slhg --icons";
        lsa = "exa -Slhga --icons";
        tree = "exa -SlhgT --icons";
        bat0 = "upower -i /org/freedesktop/UPower/devices/battery_BAT0";
        usbStat = "watch rg -e Dirty: -e Writeback: /proc/meminfo";

        # Application-related
        tmc = "emacsclient -t";
        emc = "emacsclient -c";
        zoom = "firejail zoom";
        ytv = "youtube-dl --best-quality";
        yta = "youtube-dl -x --audio-format mp3";

        # VPN
        wup = "systemctl start wg-quick-akkadianVPN.service";
        wud = "systemctl stop wg-quick-akkadianVPN.service";

        # NixOS
        flkup = "pushd ~/git/Icy-Thought/Snowflake ; nix flake update ; popd";
        thkup =
          "pushd ~/git/Icy-Thought/Snowflake ; sudo nixos-rebuild switch --flake '.#thinkpad-e595' --impure ; popd";
        proup =
          "pushd ~/git/Icy-Thought/Snowflake ; sudo nixos-rebuild switch --flake '.#probook-440g3' --impure ; popd";
        g2nix =
          "dconf dump / | dconf2nix > ~/git/Icy-Thought/Snowflake/config/dconf/gnome.nix";
      };
    };
  };
}
