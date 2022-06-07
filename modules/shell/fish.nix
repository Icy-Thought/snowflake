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
  fishCfg = "${config.snowflake.configDir}/fish";
in {
  options.modules.shell.fish = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.fish.enable {
    user.packages = with pkgs;
      [
        any-nix-shell
        fzf
        pwgen
        yt-dlp

        # Unix-tools alternatives
        bottom
        exa
        fd
        (ripgrep.override {
          withPCRE2 = true;
        })
        zoxide
      ]
      ++ (with fishPlugins; [
        done # notifies on success/failure/
        pisces # auto-pair symbols
        fzf-fish # fzf completion
      ]);

    # Several Term-UI's + Ricing:
    modules.shell = {
      neofetch.enable = true;
    };

    # Enables vendor completion: https://nixos.wiki/wiki/Fish
    programs.fish = {
      enable = true;
      interactiveShellInit = ''
        ${getExe pkgs.starship} init fish | source
        ${getExe pkgs.zoxide} init fish | source
        ${getExe pkgs.any-nix-shell} fish | source
        ${builtins.readFile "${fishCfg}/interactive.fish"}
        ${builtins.readFile "${fishCfg}/abbreviations/main.fish"}
        ${builtins.readFile "${fishCfg}/aliases/main.fish"}

        ${optionalString cfg.git.enable ''
          ${builtins.readFile "${fishCfg}/abbreviations/git.fish"}
        ''}
      '';
    };
  };
}
