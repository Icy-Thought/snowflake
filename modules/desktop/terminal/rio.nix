{ options, config, lib, pkgs, ... }:

let active = config.modules.themes.active;
in with lib; {
  options.modules.desktop.terminal.rio = {
    enable = mkEnableOption "A Rust/WebGPU based terminal emulator.";
  };

  config = mkIf config.modules.desktop.terminal.rio.enable {
    modules.shell.toolset.tmux.enable = true;

    hm.programs.rio = {
      enable = true;

      settings = mkMerge [
        {
          cursor = "▇";
          blinking-cursor = false;
          editor = "emacsclient -c";
          padding-x = 10;

          renderer = {
            backend = "Automatic";
            performance = "High";
            disable-renderer-when-unfocused = true;
          };

          keyboard = {
            use-kitty-keyboard-protocol = false;
            disable-ctlseqs-alt = false;
          };

          navigation = {
            mode = "CollapsedTab";
            clickable = true;
            use-current-path = true;
            color-automation = [ ];
          };

          scroll = {
            multiplier = 3.0;
            divider = 1.0;
          };

          window = {
            mode = "Windowed";
            background-opacity = 0.8;
            foreground-opacity = 1.0;
            blur = false;
          };
        }
        (mkIf (active != null) {
          fonts = with config.modules.themes.font.mono; {
            family = "${family}";
            size = size;

            # extras = [{family = "";}];

            regular = {
              family = "${family}";
              style = "normal";
              weight = weightNum;
            };
            bold = {
              family = "${family}";
              style = "normal";
              weight = weightNum + 100;
            };
            italic = {
              family = "${family}";
              style = "italic";
              weight = weightNum;
            };
            bold-italic = {
              family = "${family}";
              style = "italic";
              weight = weightNum + 100;
            };
          };

          theme = "${active}";
        })
      ];
    };

    create.configFile = mkIf (active != null) {
      alacritty-conf = {
        target = "rio//themes/${active}.toml";
        source = let tomlFormat = pkgs.formats.toml { };
        in with config.modules.themes.colors.main;
        tomlFormat.generate "theme-spec" {
          colors = {
            foreground = "${types.fg}";
            background = "${types.bg}";

            cursor = "${normal.yellow}";
            tabs = "${types.bg}";
            tabs-active = "${types.border}";
            selection-foreground = "${types.highlight}";
            selection-background = "${types.bg}";

            black = "${normal.black}";
            red = "${normal.red}";
            green = "${normal.green}";
            yellow = "${normal.yellow}";
            blue = "${normal.blue}";
            magenta = "${normal.magenta}";
            cyan = "${normal.cyan}";
            white = "${normal.white}";

            dim-black = "${bright.black}";
            dim-red = "${bright.red}";
            dim-green = "${bright.green}";
            dim-yellow = "${bright.yellow}";
            dim-blue = "${bright.blue}";
            dim-magenta = "${bright.magenta}";
            dim-cyan = "${bright.cyan}";
            dim-white = "${bright.white}";

            light-black = "${bright.black}";
            light-red = "${bright.red}";
            light-green = "${bright.green}";
            light-yellow = "${bright.yellow}";
            light-blue = "${bright.blue}";
            light-magenta = "${bright.magenta}";
            light-cyan = "${bright.cyan}";
            light-white = "${bright.white}";
          };
        };
      };
    };
  };
}
