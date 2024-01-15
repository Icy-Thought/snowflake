{
  config,
  options,
  lib,
  pkgs,
  ...
}: let
  inherit (lib.meta) getExe;
  inherit (lib.modules) mkIf;
in {
  options.modules.desktop.editors.helix = let
    inherit (lib.options) mkEnableOption;
  in {enable = mkEnableOption "post-modern text editor";};

  config = mkIf config.modules.desktop.editors.helix.enable (let
    inherit (config.modules.themes) editor active;
    activeTheme =
      if (active != null)
      then "${editor.helix.dark}"
      else "github-dark";
  in {
    hm.programs.helix = {
      enable = true;
      package = pkgs.helix;

      languages = {
        language = [
          {name = "latex";}
          {
            name = "haskell";
            formatter.command = "stylish-haskell";
          }
          {name = "rust";}
        ];
        language-server = {
          nil = {
            command = getExe pkgs.nil;
            config.nil.formatting.command = ["${getExe pkgs.alejandra}" "-q"];
          };
        };
      };

      settings = {
        theme = editor.helix.dark + "-alpha";
        editor = {
          lsp.display-inlay-hints = true;

          true-color = true;
          color-modes = true;
          line-number = "relative";

          cursorline = true;
          cursor-shape = {
            insert = "bar";
            normal = "block";
            select = "underline";
          };

          whitespace = {
            render = {
              space = "none";
              tab = "all";
              newline = "none";
            };
            characters = {
              space = "·";
              nbsp = "⍽";
              tab = "⇥";
              newline = "⏎";
              tabpad = "·";
            };
          };

          indent-guides = {
            character = "▏";
            rainbow = "normal";
            render = true;
          };
        };

        keys.normal = {
          space.l = {f = ":format";};
          space.o = {
            w = ":set whitespace.render all";
            W = ":set whitespace.render none";
          };
          space.w = {f = ":w";};
          space.q = {q = ":q";};
          space.space = "file_picker";
        };
      };
    };

    home.configFile.helix-theme = {
      target = "helix/themes/${activeTheme}-alpha.toml";
      source = let
        tomlFormat = pkgs.formats.toml {};
      in
        tomlFormat.generate "helix-theme" {
          inherits = "${activeTheme}";
          ui.background = {};
        };
    };
  });
}
