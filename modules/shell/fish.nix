{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.shell.fish;
  fishCfg = "${config.snowflake.configDir}/fish";
in {
  options.modules.shell.fish = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      any-nix-shell
      fzf
      pwgen
      yt-dlp

      # alternatives for several gnu-tools
      bottom
      exa
      fd
      (ripgrep.override {
        withPCRE2 = true;
      })
      zoxide
    ];

    # Enables vendor completion: https://nixos.wiki/wiki/Fish
    programs.fish.enable = true;

    hm.programs.fish = {
      enable = true;
      # useBabelfish = true;
      interactiveShellInit = ''
        ${getExe pkgs.starship} init fish | source
        ${getExe pkgs.zoxide} init fish | source
        ${getExe pkgs.any-nix-shell} fish | source
        ${builtins.readFile "${fishCfg}/interactive.fish"}
        ${builtins.readFile "${fishCfg}/abbreviations/main.fish"}
        ${builtins.readFile "${fishCfg}/aliases/main.fish"}

        ${optionalString config.modules.shell.git.enable ''
          ${builtins.readFile "${fishCfg}/abbreviations/git.fish"}
        ''}
      '';

      plugins = with pkgs.fishPlugins; [
        {
          name = "done";
          src = done.src;
        }
        {
          name = "pisces";
          src = pisces.src;
        }
        {
          name = "fzf-fish";
          src = fzf-fish.src;
        }
      ];
    };
  };
}
