{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.shell;
  configDir = config.snowflake.configDir;
in {
  options.modules.shell.fish = {enable = mkBoolOpt false;};

  config = mkIf cfg.fish.enable {
    users.defaultUserShell = pkgs.fish;

    user.packages = with pkgs; [
      any-nix-shell
      fzf
      pipes-rs
      pwgen
      yt-dlp

      # Unix-tools alternatives
      bat
      bottom
      duf
      exa
      fd
      ripgrep
      zoxide
    ];

    # Several Term-UI's + Ricing:
    modules.shell = {
      colorPanes.enable = true;
      neofetch.enable = true;
    };

    # Enables vendor completion: https://nixos.wiki/wiki/Fish
    programs.fish.enable = true;

    home.programs.fish = {
      enable = true;
      interactiveShellInit = ''
        ${getExe pkgs.starship} init fish | source
        ${getExe pkgs.zoxide} init fish | source
        ${getExe pkgs.any-nix-shell} fish | source
        ${builtins.readFile "${configDir}/fish/interactive.fish"}

        ${optionalString cfg.git.enable ''
          fish -c "__git.init" || true
        ''}
      '';

      functions = {
        eg = "emacs --create-frame $argv & disown";
        ecg = "emacsclient --create-frame $argv & disown";
        gitignore = "curl -sL https://www.gitignore.io/api/$argv";
      };

      plugins = [
        {
          name = "done";
          src = pkgs.fetchFromGitHub {
            owner = "franciscolourenco";
            repo = "done";
            rev = "d6abb267bb3fb7e987a9352bc43dcdb67bac9f06";
            sha256 = "1h8v5jg9kkali50qq0jn0i1w68wp4c2l0fapnglnnpg0v4vv51za";
          };
        }
        {
          name = "fzf";
          src = pkgs.fetchFromGitHub {
            owner = "PatrickF1";
            repo = "fzf.fish";
            rev = "628f04bf239bc6b820c090b8c814ca3c242142d7";
            sha256 = "EiNaEhqJbisv/gA5th2IWh4XIQ3zlGrXTpMMTaUb0Ag=";
          };
        }
        {
          name = "fish-abbreviation-tips";
          src = pkgs.fetchFromGitHub {
            owner = "Gazorby";
            repo = "fish-abbreviation-tips";
            rev = "d29a52375a0826ed86b0710f58b2495a73d3aff3";
            sha256 = "0s6zcxlhfys545lnfg626ilk1jqgak9xpijy3jxs9z12w2c4d3gk";
          };
        }
        (mkIf cfg.git.enable {
          name = "git";
          src = pkgs.fetchFromGitHub {
            owner = "jhillyerd";
            repo = "plugin-git";
            rev = "1a0357c1f13a9c5f18a5d773e9c0c963f1ff23b6";
            sha256 = "d1Mcn+u9aLI3PLDXth0VyoqZiO6Z6R/yL7f/RwIR0/o=";
          };
        })
      ];
    };
  };
}
