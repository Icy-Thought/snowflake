{ config, pkgs, inputs, ... }:

let homeDir = config.home.homeDirectory;

in {
  imports = [
    ./git
    ./fish
    ./alacritty
    ./tmux
    ./emacs
    ./neovim
    ./zathura
    ./mpd
    ./ncmpcpp
    ./packages.nix
  ];

  programs = {
    home-manager = {
      enable = true;
      path = "${config.home.homeDirectory}/.nixpkgs/modules/home-manager";
    };

    bash = {
      enable = true;
      shellAliases = { ls = "ls --color=auto"; };
      bashrcExtra = ''
        eval "$(starship init bash)"
      '';
    };

    direnv = {
      enable = true;
      enableFishIntegration = true;
      nix-direnv.enable = true;
    };

    htop = {
      enable = true;
      settings = {
        color_scheme = 0;
        enable_mouse = true;
        show_program_path = false;
        sort_direction = 1;
        sort_key = 46;

        left_meters = [ "AllCPUs" "Memory" "Swap" ];
        left_meter_modes = [ 1 1 1 ];
        right_meters = [ "Tasks" "LoadAverage" "Uptime" ];
        left_emter_modes = [ 2 2 2 ];
      };
    };

  };

  fonts.fontconfig.enable = true;

  home = {
    sessionVariables = { MOZ_ENABLE_WAYLAND = 1; };

    sessionPath = [
      "${config.home.homeDirectory}/.emacs.d/bin"
      "${config.home.homeDirectory}/.cargo/bin"
      "${config.home.homeDirectory}/.local/bin"
      "${config.home.homeDirectory}/go/bin"
    ];
  };

}
