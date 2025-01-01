{ options, config, lib, pkgs, ... }:

with lib; {
  options.modules.shell.toolset.fzf = {
    enable = mkEnableOption "TUI Fuzzy Finder.";
  };

  config = mkIf config.modules.shell.toolset.fzf.enable {
    hm.programs.fzf = {
      enable = true;
      enableBashIntegration = true;
      enableZshIntegration = config.modules.shell.default == "zsh";
      enableFishIntegration = config.modules.shell.default == "fish";

      tmux.enableShellIntegration = true;
      tmux.shellIntegrationOptions = [ "-d 40%" ];

      defaultCommand = "fd --type f";
      defaultOptions = [ "--height 40%" "--border" ];

      changeDirWidgetCommand = "fd --type d";
      changeDirWidgetOptions = [ "--preview 'tree -C {} | head -200'" ];

      fileWidgetCommand = "fd --type f";
      fileWidgetOptions = [ "--preview 'head {}'" ];
      historyWidgetOptions = [ "--sort" "--exact" ];

      colors = with config.modules.themes.colors.main;
        mkIf (config.modules.themes.active != null) {
          bg = "${types.bg}";
          "bg+" = "${types.bg}";
          fg = "${types.border}";
          "fg+" = "${types.border}";
          hl = "${normal.red}";
          "hl+" = "${normal.red}";
          header = "${normal.red}";
          marker = "${normal.magenta}";
          info = "${normal.magenta}";
          prompt = "${types.border}";
          spinner = "${types.panelbg}";
          pointer = "${types.border}";
        };
    };
  };
}
