{ config, options, lib, pkgs, ... }:
let
  inherit (lib.attrsets) optionalAttrs;
  inherit (lib.modules) mkIf;
in {
  options.modules.shell.toolset.fzf = let inherit (lib.options) mkEnableOption;
  in { enable = mkEnableOption "TUI Fuzzy Finder."; };

  config = mkIf config.modules.shell.toolset.fzf.enable {
    hm.programs.fzf = let defShell = config.modules.shell.default;
    in {
      enable = true;
      enableBashIntegration = true;
      enableZshIntegration = defShell == "zsh";
      enableFishIntegration = defShell == "fish";

      tmux.enableShellIntegration = true;
      tmux.shellIntegrationOptions = [ "-d 40%" ];

      defaultCommand = "fd --type f";
      defaultOptions = [ "--height 40%" "--border" ];

      changeDirWidgetCommand = "fd --type d";
      changeDirWidgetOptions = [ "--preview 'tree -C {} | head -200'" ];

      fileWidgetCommand = "fd --type f";
      fileWidgetOptions = [ "--preview 'head {}'" ];
      historyWidgetOptions = [ "--sort" "--exact" ];

      colors = let
        inherit (config.modules.themes) active;
        inherit (config.modules.themes.colors.main) normal types;
      in mkIf (active != null) {
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
