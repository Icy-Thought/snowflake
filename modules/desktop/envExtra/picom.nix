{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.envExtras.picom;
in {
  options.modules.desktop.envExtras.picom = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    services.picom.enable = true;
    services.picom.backend = "glx";
    services.picom.experimentalBackends = true;

    services.picom.inactiveOpacity = 0.8;
    services.picom.opacityRules = [
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

    services.picom.menuOpacity = 0.9;

    services.picom.fade = true;
    services.picom.fadeDelta = 10;
    services.picom.fadeSteps = [ (3.0e-2) (3.0e-2) ];
    services.picom.fadeExclude = [
      "window_type = 'combo'"
      "window_type = 'desktop'"
      "window_type = 'dock'"
      "window_type = 'dnd'"
      "window_type = 'notification'"
      "class_g = 'Rofi'"
      "class_g = 'slop'"
    ];

    services.picom.refreshRate = 0;

    services.picom.shadow = true;
    services.picom.shadowExclude = [
      "_GTK_FRAME_EXTENTS@:c"
      "window_type = 'desktop'"
      "window_type = 'dock'"
      "window_type = 'dnd'"
      "window_type = 'notification'"
      "window_type = 'toolbar'"
      "window_type = 'utility'"
      "_COMPTON_SHADOW@:32c = 0"
      "class_g = 'Rofi'"
    ];

    services.picom.shadowOffsets = [ (-7) (-7) ];
    services.picom.shadowOpacity = 0.75;
    services.picom.vSync = true;

    services.picom.settings = {
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
}
