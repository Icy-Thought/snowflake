{ config, pkgs, inputs, ... }:

let home = config.home.homeDirectory;

in {
  imports = [
    ./pkgs
    ./config/git
    ./config/fish
    ./config/alacritty
    ./config/tmux
    ./config/emacs
    ./config/neovim
    ./config/zathura
    ./config/mpd
    ./config/ncmpcpp
    ./config/ungoogled-chromium
  ];

  programs = {
    home-manager = {
      enable = true;
      path = "${home}/.nixpkgs/modules/home-manager";
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

  home.sessionPath = [
    "${home}/.emacs.d/bin"
    "${home}/.cargo/bin"
    "${home}/.local/bin"
    "${home}/go/bin"
  ];

}
