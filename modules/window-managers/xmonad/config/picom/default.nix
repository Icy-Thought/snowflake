{ config, lib, pkgs, ... }: {

  services.picom = {
    enable = true;
    backend = "glx";
    experimentalBackends = true;

    inactiveOpacity = 1.0;
    opacityRules = [ "100:class_g = 'firefox' && argb" ];
    menuOpacity = 0.9;

    fade = true;
    fadeDelta = 10;
    fadeExclude = [ ];
    fadeSteps = [ (0.59) (0.59) ];

    refreshRate = 0;

    shadow = true;
    shadowExclude = [
      "name = 'Notification'"
      "class_g = 'Conky'"
      "class_g ?= 'Notify-osd'"
      "class_g = 'Cairo-clock'"
      "class_g = 'slop'"
      "name = 'hacksaw'"
      # "class_g = 'scrot'"
      "class_g = 'Polybar'"
      "class_g = 'trayer'"
      "class_g = 'stalonetray'"
      "fullscreen"
      "! name~=''"
      "!WM_CLASS:s"
      "_GTK_FRAME_EXTENTS@:c"
      "_NET_WM_STATE@:32a *= '_NET_WM_STATE_HIDDEN'"
    ];

    shadowOffsets = [ (-8) (-8) ];
    shadowOpacity = 0.7;

    vSync = false;

    settings = {
      ### Corners ###
      corner-radius = 12.0;
      round-borders = 1;
      round-borders-exclude = [ ];
      rounded-corners-exclude = [ "class_g = 'Polybar'" ];

      ### Shadows ###
      shadow-radius = 8;
      shadow-color = "#000000";

      ### Fading ###
      no-fading-openclose = false;
      no-fading-destroyed-argb = true;

      ### Transparency / Opacity ###
      frame-opacity = 1.0;
      inactive-opacity-override = false;

      focus-exclude = [
        "class_g ?= 'rofi'"
        "class_g ?= 'slop'"
        "class_g ?= 'Steam'"
        "class_g = 'Cairo-clock'"
      ];

      ### Background-Blur ###
      blur = {
        method = "kawase";
        strength = "5.0";
      };

      blur-background = false;
      blur-background-frame = true;
      blur-background-fixed = true;

      blur-background-exclude = [ "class_g = 'slop'" "_GTK_FRAME_EXTENTS@:c" ];

      ### General ###
      daemon = false;
      dbus = false;
      mark-wmwin-focused = true;
      mark-ovredir-focused = true;
      detect-rounded-corners = true;
      detect-client-opacity = true;

      unredir-if-possible-exclude = [ ];
      detect-transient = true;
      detect-client-leader = true;
      resize-damage = 1;

      invert-color-include = [ ];
      glx-no-stencil = true;
      use-damage = true;
      transparent-clipping = false;

      log-level = "warn";
      log-file = "~/.cache/picom-log.log";
      show-all-xerrors = true;

      wintypes = {
        tooltip = {
          fade = true;
          focus = false;
          opacity = 1;
          full-shadow = false;
        };

        normal = { };
        dock = {
          shadow = true;
          opacity = 1.0;
        };

        popup_menu = {
          shadow = true;
          focus = false;
          opacity = 0.9;
        };

        dropdown_menu = {
          opacity = 1.0;
          focus = false;
        };

        above = { };
        splash = { };

        utility = {
          focus = false;
          opacity = 1.0;
          blur-background = false;
        };

        notification = { };
        desktop = { blur-background = false; };
        menu = { focus = false; };
        dialog = { };
      };
    };
  };
}
