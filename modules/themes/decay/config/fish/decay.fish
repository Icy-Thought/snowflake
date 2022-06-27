# --> General
set -l foreground b6beca
set -l selection 70a5eb

# --> palette
set -l magenta c68aee
set -l pink fc54b6
set -l red e05f65
set -l peach fc6868
set -l green 94f7c5
set -l yellow f1cf8a
set -l blue 70a5eb
set -l white dee1e6

# Syntax Highlighting
set -g fish_color_normal $foreground
set -g fish_color_command $blue
set -g fish_color_param $green
set -g fish_color_keyword $red
set -g fish_color_quote $green
set -g fish_color_redirection $pink
set -g fish_color_end $peach
set -g fish_color_error $red
set -g fish_color_gray $white
set -g fish_color_selection --background=$selection
set -g fish_color_search_match --background=$selection
set -g fish_color_operator $pink
set -g fish_color_autosuggestion $white
set -g fish_color_cancel $red

# Prompt
set -g fish_color_cwd $yellow
set -g fish_color_host $blue

# Completion Pager
set -g fish_pager_color_progress $white
set -g fish_pager_color_prefix $pink
set -g fish_pager_color_completion $foreground
set -g fish_pager_color_description $white
