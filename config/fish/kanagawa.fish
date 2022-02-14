# --> General
set -l foreground dcd7ba
set -l selection 2d4f67

# --> palette
set -l red c34043
set -l pink d27e99
set -l orange ff9e64
set -l yellow c0a36e
set -l green 76946a
set -l cyan 7aa89f
set -l purple 957fb8
set -l gray 727169

# Syntax Highlighting
set -g fish_color_normal $foreground
set -g fish_color_command $purple
set -g fish_color_param $purple
set -g fish_color_keyword $red
set -g fish_color_quote $green
set -g fish_color_redirection $pink
set -g fish_color_end $orange
set -g fish_color_error $red
set -g fish_color_gray $gray
set -g fish_color_selection --background=$selection
set -g fish_color_search_match --background=$selection
set -g fish_color_operator $pink
set -g fish_color_escape $pink
set -g fish_color_autosuggestion $gray
set -g fish_color_cancel $red

# Prompt
set -g fish_color_cwd $yellow
set -g fish_color_user $cyan
set -g fish_color_host $purple

# Completion Pager
set -g fish_pager_color_progress $gray
set -g fish_pager_color_prefix $pink
set -g fish_pager_color_completion $foreground
set -g fish_pager_color_description $gray
