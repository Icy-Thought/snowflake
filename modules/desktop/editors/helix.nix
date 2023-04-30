{ config, options, lib, pkgs, ... }:

let inherit (lib.modules) mkIf;
in {
  options.modules.desktop.editors.helix =
    let inherit (lib.options) mkEnableOption;
    in { enable = mkEnableOption false; };

  config = mkIf config.modules.desktop.editors.helix.enable {
    hm.programs.helix = {
      enable = true;
      package = pkgs.helix;

      languages = [
        { name = "latex"; }
        {
          name = "haskell";
          formatter.command = "stylish-haskell";
        }
        {
          name = "nix";
          language-server.command = "nil";
          formatter.command = "nixpkgs-fmt";
        }
        { name = "rust"; }
      ];

      settings = {
        theme = config.modules.themes.editor.helix.dark;
        editor = {
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
              tab = "→";
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
          space.l = { f = ":format"; };
          space.w = { f = ":w"; };
          space.q = { q = ":q"; };
          space.space = "file_picker";
        };
      };
    };
  };
}
