{ config
, options
, lib
, pkgs
, inputs
, ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.editors.helix;
  colorscheme = config.modules.themes.helix.theme;
in
{
  options.modules.desktop.editors.helix = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays = [ inputs.helix.overlay ];

    programs.helix = {
      enable = true;
      package = pkgs.helix;

      languages = [
        { name = "latex"; }
        {
          name = "haskell";
          formatter = {
            command = "stylish-haskell";
            args = [ "--stdin" ];
          };
        }
        {
          name = "nix";
          formatter = {
            command = "nixpkgs-fmt";
            args = [ "--stdin" ];
          };
        }
        { name = "rust"; }
      ];

      settings = {
        theme = colorscheme;
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
              space = "all";
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
            render = true;
            character = "╎";
          };
        };

        keys.normal = {
          space.l = { f = ":format"; };
          space.w = { f = ":w"; };
          space.q = { q = ":q"; };
        };
      };
    };
  };
}
