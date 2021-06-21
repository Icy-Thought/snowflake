{ config, pkgs, inputs, ... }:
{
  imports = [
    ./hm-packages.nix
    ./modules/git.nix
    ./modules/fish.nix
    ./modules/alacritty.nix
    ./modules/emacs.nix
    ./modules/neovim.nix
    ./modules/zathura.nix
  ];

  nixpkgs = {
    config = {
      allowUnfree = true;
    };

    overlays = [
      (import ../overlays/neovim-nightly-overlay.nix)
      (import ../overlays/mozilla-overlay.nix)
      (import ../overlays/rust-overlay.nix)
    ];

  };

  programs = {
    home-manager = {
      enable = true;
    };

    bash = {
      enable = true;
      shellAliases = {
        ls = "ls --color=auto";
      };
      bashrcExtra = ''
        eval "$(starship init bash)"
      '';
    };

    tmux = {
      enable = true;
      baseIndex = 1;
      extraConfig = builtins.readFile ./config/tmux/tmux.conf;
      escapeTime = 0;
      keyMode = "vi";
      shortcut = "a";
      terminal = "screen-256color";
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
 
  fonts = {
    fontconfig = {
      enable = true;
    };
  };

  home = {
    sessionPath = [ 
      "${config.xdg.configHome}/.cargo/bin"
      "${config.xdg.configHome}/.local/bin"
      "${config.xdg.configHome}/.emacs/bin"
      "${config.xdg.configHome}/go/bin"
    ];

    sessionVariables = {
      DOOMDIR = "${config.home.homeDirectory}/git/NixOS-Configurations/home/config/doom.d";
    };

    file = {
      ".doom.d" = {
        source = ./config/doom.d;
        recursive = true;
        onChange = "doom -y sync -u";
      };
    };
  };

}
