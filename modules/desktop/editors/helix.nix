{ config, options, lib, pkgs, ... }:
let
  inherit (lib.meta) getExe;
  inherit (lib.modules) mkIf;
in {
  options.modules.desktop.editors.helix =
    let inherit (lib.options) mkEnableOption;
    in { enable = mkEnableOption "post-modern text editor"; };

  config = mkIf config.modules.desktop.editors.helix.enable (let
    inherit (config.modules.themes) editor active;
    activeTheme =
      if (active != null) then "${editor.helix.dark}" else "github-dark";
  in {
    hm.programs.helix = {
      enable = true;
      package = pkgs.helix;

      languages = {
        language = [
          { name = "latex"; }
          {
            name = "haskell";
            formatter.command = "stylish-haskell";
          }
          { name = "rust"; }
        ];
        language-server = {
          nil = {
            command = getExe pkgs.nil;
            config.nil.formatting.command = [ "${getExe pkgs.alejandra}" "-q" ];
          };
        };
      };

      settings = {
        theme = editor.helix.dark + "-alpha";
        editor = {
          true-color = true;
          color-modes = true;
          idle-timeout = 1;
          line-number = "relative";
          mouse = true;
          scrolloff = 5;
          bufferline = "always";

          auto-completion = true;
          auto-format = true;
          lsp.display-inlay-hints = true;

          cursorline = true;
          cursor-shape = {
            insert = "bar";
            normal = "block";
            select = "underline";
          };

          indent-guides = {
            character = "▏";
            rainbow = "normal";
            render = true;
          };
          gutters = [ "diagnostics" "line-numbers" "spacer" "diff" ];

          statusline = {
            separator = "";
            left = [
              "mode"
              "selections"
              "spinner"
              "file-name"
              "total-line-numbers"
            ];
            center = [ ];
            right = [
              "diagnostics"
              "file-encoding"
              "file-line-ending"
              "file-type"
              "position-percentage"
              "position"
            ];
            mode = {
              normal = "NORMAL";
              insert = "INSERT";
              select = "SELECT";
            };
          };

          whitespace.characters = {
            space = "·";
            nbsp = "⍽";
            tab = "⇥";
            newline = "⏎";
            tabpad = "·";
          };
        };

        keys.normal = {
          space.w = {
            a = ":set whitespace.render all";
            n = ":set whitespace.render none";
          };
          space.f = {
            f = ":format";
            s = ":w";
            q = ":bc";
          };
          space.space = "file_picker";
        };
      };
    };

    create.configFile.helix-theme = {
      target = "helix/themes/${activeTheme}-alpha.toml";
      text = ''
        inherits = "${activeTheme}"
        "ui.background" = {}
      '';
    };
  });
}
