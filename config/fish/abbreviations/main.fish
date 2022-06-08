# General
abbr -ag ls "exa -Slhg --icons"
abbr -ag lsa "exa -Slhga --icons"
abbr -ag tree "exa -SlhgT --icons"
abbr -ag emc "emacsclient -c"
abbr -ag tmc "emacsclient -t"
abbr -ag usbStat "watch rg -e Dirty: -e Writeback: /proc/meminfo"
abbr -ag wget "curl -O"

# Nix
abbr -ag nb "nix-build -E 'with import <nixpkgs> {}; callPackage ./. {}'"
abbr -ag np "nix-shell -p"
abbr -ag nls "nix-store --query --requisites /run/current-system | cut -d- -f2- | sort | uniq"

# Sys-Management
abbr -ag bat0 "upower -i /org/freedesktop/UPower/devices/battery_BAT0"
abbr -ag flkup "nix flake update"
abbr -ag thkup "nixos-rebuild switch --use-remote-sudo --flake .#thinkpad-e595 --impure"
abbr -ag proup "nixos-rebuild switch --use-remote-sudo --flake .#probook-440g3 --impure"
abbr -ag d2nix "dconf dump / | dconf2nix > dconf.nix"

# Other
abbr -ag wud "systemctl stop wg-quick-akkadianVPN.service"
abbr -ag wup "systemctl start wg-quick-akkadianVPN.service"
abbr -ag yta "youtube-dl -x --audio-format mp3"
abbr -ag ytv "youtube-dl --best-quality"
