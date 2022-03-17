# General configurations
set fish_greeting
set -g fish_key_bindings fish_vi_key_bindings

# Customizable fish_title
function fish_title
    echo $argv[1]
end

# Tmux on fish start
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

# Aliases
alias exa 'exa --group-directories-first'

# Abbreviations
## Sys-Management
abbr --add --global -- bat0 'upower -i /org/freedesktop/UPower/devices/battery_BAT0'
abbr --add --global -- flkup 'pushd ~/git/Icy-Thought/Snowflake ; nix flake update ; popd'
abbr --add --global -- thkup 'pushd ~/git/Icy-Thought/Snowflake ; sudo nixos-rebuild switch --flake '\''.#thinkpad-e595'\'' --impure ; popd'
abbr --add --global -- proup 'pushd ~/git/Icy-Thought/Snowflake ; sudo nixos-rebuild switch --flake '\''.#probook-440g3'\'' --impure ; popd'
abbr --add --global -- g2nix 'dconf dump / | dconf2nix > ~/git/Icy-Thought/Snowflake/config/dconf/gnome.nix'

## General
abbr --add --global -- ls 'exa -Slhg --icons'
abbr --add --global -- lsa 'exa -Slhga --icons'
abbr --add --global -- tree 'exa -SlhgT --icons'
abbr --add --global -- emc 'emacsclient -c'
abbr --add --global -- tmc 'emacsclient -t'
abbr --add --global -- usbStat 'watch rg -e Dirty: -e Writeback: /proc/meminfo'

## Nix
abbr --add --global -- nb 'nix-build -E \'with import <nixpkgs> {}; callPackage ./. {}\''
abbr --add --global -- np 'nix-shell -p'

## Other
abbr --add --global -- wud 'systemctl stop wg-quick-akkadianVPN.service'
abbr --add --global -- wup 'systemctl start wg-quick-akkadianVPN.service'
abbr --add --global -- yta 'youtube-dl -x --audio-format mp3'
abbr --add --global -- ytv 'youtube-dl --best-quality'
abbr --add --global -- zoom 'firejail zoom'
