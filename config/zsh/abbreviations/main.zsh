# General
abbr ls="exa -Slhg --icons"
abbr lsa="exa -Slhga --icons"
abbr tree="exa -SlhgT --icons"
abbr emc="emacsclient -c"
abbr tmc="emacsclient -t"
abbr usbStat="watch rg -e Dirty: -e Writeback: /proc/meminfo"
abbr wget="curl -O"

# Nix
abbr nb="nix-build -E 'with import <nixpkgs> {}; callPackage ./. {}'"
abbr np="nix-shell -p"
abbr nls="nix-store --query --requisites /run/current-system | cut -d- -f2- | sort | uniq"

# Sys-Management
abbr bat0="upower -i /org/freedesktop/UPower/devices/battery_BAT0"
abbr flkup="nix flake update"
abbr thkup="nixos-rebuild switch --use-remote-sudo --flake .#thinkpad-e595 --impure"
abbr proup="nixos-rebuild switch --use-remote-sudo --flake .#probook-440g3 --impure"
abbr d2nix="dconf dump / | dconf2nix > dconf.nix"

# Other
abbr wud="systemctl stop wg-quick-akkadianVPN.service"
abbr wup="systemctl start wg-quick-akkadianVPN.service"
abbr yta="youtube-dl -x --audio-format mp3"
abbr ytv="youtube-dl --best-quality"
