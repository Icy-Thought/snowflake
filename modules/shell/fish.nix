{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.fish;
in {
  options.modules.shell.fish = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    users.defaultUserShell = pkgs.fish;

    user.packages = with pkgs; [
      exa
      skim
      ripgrep
      youtube-dl
      any-nix-shell
      pipes-rs
      pwgen
      bat
      fd
    ];

    homeManager.programs.fish = {
      enable = true;

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
        # if status is-interactive
        # and not set -q TMUX
        #     exec tmux
        # end

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

        # Git
        g = "git";
        gc = "git clone";
        ga = "git add";
        gaa = "git add -A";
        gcm = "git commit -m";
        gps = "git push";
        gpl = "git pull";
        gs = "git status";

        # NixOS
        flkup = "pushd ~/git/Icy-Thought/Snowflake ; nix flake update ; popd";
        thkup =
          "sudo nixos-rebuild switch --flake '/etc/nixos#thinkpad-e595' --impure";
        proup =
          "sudo nixos-rebuild switch --flake '/etc/nixos#probook-440g3' --impure";
        g2nix =
          "dconf dump / | dconf2nix > ~/git/Icy-Thought/Snowflake/config/dconf/gnome.nix";
      };
    };
  };
}
