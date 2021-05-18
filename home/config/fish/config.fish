# General Configurations
set fish_greeting
set -gx EDITOR nvim
set -g fish_key_bindings fish_vi_key_bindings

# Customizable fish_title
function fish_title
    echo $argv[1]
end

# Start TMUX on Terminal Start
# if status is-interactive
# and not set -q TMUX
#     exec tmux
# end

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

# Defining Paths
set PATH $HOME/.cargo/bin $PATH
set PATH $HOME/.emacs.d/bin $PATH
set PATH $HOME/go/bin $PATH
set PATH $HOME/.local/bin $PATH

# Sources
starship init fish | source
