{ config, pkgs, ... }:

let
  print-colors = pkgs.writeScriptBin "print-colors" ''
    T='•••'   # The test text

    echo -e "\n                 40m     41m     42m     43m\
         44m     45m     46m     47m";

    for FGs in '    m' '   1m' '  30m' '1;30m' '  31m' '1;31m' '  32m' \
               '1;32m' '  33m' '1;33m' '  34m' '1;34m' '  35m' '1;35m' \
               '  36m' '1;36m' '  37m' '1;37m';
      do FG='$'{FGs// /}
      echo -en " $FGs \033[$FG  $T  "
      for BG in 40m 41m 42m 43m 44m 45m 46m 47m;
        do echo -en "$EINS \033[$FG\033[$BG  $T  \033[0m";
      done
      echo;
    done
  '';

in {
  home.packages = [ print-colors ];

  programs.fish = {
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

      # Colorscheme: Ayu-dark
      if test "$TERM" != "linux"
         set -U fish_color_autosuggestion 4D5566
         set -U fish_color_cancel -r
         set -U fish_color_command 39BAE6
         set -U fish_color_comment 626A73
         set -U fish_color_cwd 59C2FF
         set -U fish_color_cwd_root red
         set -U fish_color_end F29668
         set -U fish_color_error FF3333
         set -U fish_color_escape 95E6CB
         set -U fish_color_history_current --bold
         set -U fish_color_host normal
         set -U fish_color_match F07178
         set -U fish_color_normal B3B1AD
         set -U fish_color_operator E6B450
         set -U fish_color_param B3B1AD
         set -U fish_color_quote C2D94C
         set -U fish_color_redirection FFEE99
         set -U fish_color_search_match --background=E6B450
         set -U fish_color_selection --background=E6B450
         set -U fish_color_user brgreen
         set -U fish_color_valid_path --underline
         set -U fish_pager_color_completion normal
         set -U fish_pager_color_description B3A06D yellow
         set -U fish_pager_color_prefix normal --bold --underline
         set -U fish_pager_color_progress brwhite --background=cyan
      end

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
      bat0 = "upower -i /org/freedesktop/UPower/devices/battery_BAT0";

      # Application-related
      temacs = "emacsclient -t";
      emacs = "emacsclient -c -a";
      dup = "doom upgrade && doom sync -u";
      zoom = "firejail zoom";
      ytv = "youtube-dl --best-quality";
      yta = "youtube-dl -x --audio-format mp3";

      # VPN
      wup = "systemctl start wg-quick-Akkadian-VPN.service";
      wud = "systemctl stop wg-quick-Akkadian-VPN.service";

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
      flup = "nix flake update";
      genup =
        "sudo nixos-rebuild switch --flake '/etc/nixos#thinkpad' --impure";
      g2nix =
        "dconf dump / | dconf2nix > ~/git/Icy-Thought/Snowflake/config/dconf/gnome.nix";

    };
  };

}
