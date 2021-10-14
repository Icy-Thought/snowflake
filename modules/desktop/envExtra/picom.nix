{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.envExtra.picom;
in {
  options.modules.desktop.envExtra.picom = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    # TODO: after wintypes merge -> homeManager.
    services.picom = {
      enable = true;
      backend = "glx";
      experimentalBackends = true;

      inactiveOpacity = 0.8;
      opacityRules = [
        "100:window_type = 'combo'"
        "100:window_type = 'desktop'"
        "100:window_type = 'dialog'"
        "100:window_type = 'dock'"
        "100:window_type = 'dnd'"
        "100:window_type = 'dropdown_menu'"
        "100:window_type = 'toolbar'"
        "100:window_type = 'menu'"
        "100:window_type = 'normal'"
        "100:window_type = 'notification'"
        "100:window_type = 'popup_menu'"
        "100:window_type = 'splash'"
        "100:window_type = 'toolbar'"
        "100:window_type = 'utility'"
        "100:window_type = 'unknown'"
      ];

      menuOpacity = 0.9;

      fade = true;
      fadeDelta = 10;
      fadeSteps = [ (3.0e-2) (3.0e-2) ];
      fadeExclude = [
        "window_type = 'combo'"
        "window_type = 'desktop'"
        "window_type = 'dock'"
        "window_type = 'dnd'"
        "window_type = 'notification'"
        "class_g = 'Rofi'"
        "class_g = 'slop'"
      ];

      refreshRate = 0;

      shadow = true;
      shadowExclude = [
        "_GTK_FRAME_EXTENTS@:c"
        "window_type = 'desktop'"
        "window_type = 'dock'"
        "window_type = 'dnd'"
        "window_type = 'notification'"
        "window_type = 'toolbar'"
        "window_type = 'utility'"
        "_COMPTON_SHADOW@:32c = 0"
        "class_g = 'Rofi'"
        "name = 'hacksaw'"
      ];

      shadowOffsets = [ (-7) (-7) ];
      shadowOpacity = 0.75;
      vSync = true;

      settings = {
        ### Background-Blur ###
        blur = {
          method = "kawase";
          strength = "7.0";
        };

        blur-background = false;
        blur-background-frame = false;
        blur-background-fixed = false;
        blur-background-exclude = [
          "_GTK_FRAME_EXTENTS@:c"
          "window_type = 'combo'"
          "window_type = 'desktop'"
          "window_type = 'dnd'"
          "window_type = 'dropdown_menu'"
          "window_type = 'toolbar'"
          "window_type = 'menu'"
          "window_type = 'notification'"
          "window_type = 'utility'"
          # "window_type = 'dock'"
          # "window_type = 'dialog'"

          "class_g = 'Rofi'"
          "class_g = 'Firefox' && window_type = 'utility'"
        ];

        ### Animations ###
        transition-length = 150;
        transition-pow-x = 0.1;
        transition-pow-y = 0.1;
        transition-pow-w = 0.1;
        transition-pow-h = 0.1;
        size-transition = true;

        ### Corners ###
        corner-radius = 10.0;
        round-borders = 1;
        round-borders-exclude = [ ];
        rounded-corners-exclude = [ "class_g = 'Dunst'" ];

        ### Shadows ###
        shadow-radius = 7;
        shadow-color = "#000000";

        ### Fading ###
        no-fading-openclose = false;
        no-fading-destroyed-argb = true;

        ### Transparency / Opacity ###
        frame-opacity = 0.7;
        inactive-opacity-override = false;
        active-opacity = 1.0;

        focus-exclude = [ "class_g ?= 'rofi'" "class_g ?= 'Steam'" ];

        ### General ###
        daemon = false;
        dbus = false;
        mark-wmwin-focused = true;
        mark-ovredir-focused = true;
        detect-rounded-corners = true;
        detect-client-opacity = true;

        unredir-if-possible = false;
        unredir-if-possible-exclude = [ ];
        detect-transient = true;
        detect-client-leader = true;

        invert-color-include = [ ];
        glx-no-stencil = true;
        use-damage = false;
        transparent-clipping = false;

        wintypes = {
          tooltip.fade = true;
          tooltip.shadow = true;
          tooltip.opacity = 0.75;
          tooltip.focus = false;
          tooltip.full-shadow = false;

          normal = { };
          dock.shadow = true;
          dock.opacity = 1.0;

          popup_menu.shadow = true;
          popup_menu.focus = false;
          popup_menu.opacity = 0.8;

          dropdown_menu.opacity = 0.8;
          dropdown_menu.focus = false;

          above = { };
          splash = { };

          utility.focus = false;
          utility.opacity = 1.0;
          utility.blur-background = false;

          notification = { };
          desktop = { blur-background = false; };
          menu = { focus = false; };
          dialog = { };
        };
      };
    };
  };
}
