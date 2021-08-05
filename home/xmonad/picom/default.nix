{ config, lib, pkgs, ... }: {

  services.picom = {
    enable = true;
    backend = "glx";
    experimentalBackends = true;

    inactiveOpacity = 0.8;
    opacityRules = [ "100:class_g = 'firefox'" ];
    menuOpacity = 0.9;

    fade = true;
    fadeDelta = 10;
    fadeSteps = [ (0.03) (0.03) ];
    fadeExclude = [ "class_g = 'slop'" ];

    refreshRate = 0;

    shadow = true;
    shadowExclude = [
      "name = 'Notification'"
      "class_g = 'Conky'"
      "class_g ?= 'Notify-osd'"
      "class_g = 'Cairo-clock'"
      "class_g = 'slop'"
      "name = 'hacksaw'"
      "class_g = 'Polybar'"
      "fullscreen"
      "! name~=''"
      "!WM_CLASS:s"
      "_GTK_FRAME_EXTENTS@:c"
      "_NET_WM_STATE@:32a *= '_NET_WM_STATE_HIDDEN'"
    ];

    shadowOffsets = [ (-7) (-7) ];
    shadowOpacity = 0.75;

    vSync = false;

    settings = {
      ### Background-Blur ###
      blur = {
        method = "kawase";
        strength = "7.0";
      };

      blur-background = false;
      blur-background-frame = false;
      blur-background-fixed = false;

      blur-background-exclude = [ "class_g = 'slop'" "_GTK_FRAME_EXTENTS@:c" ];

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

      rounded-corners-exclude = [ ];

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

      focus-exclude = [
        "class_g ?= 'rofi'"
        "class_g ?= 'slop'"
        "class_g ?= 'Steam'"
        "class_g = 'Cairo-clock'"
      ];

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

      invert-color-include = [ ];
      glx-no-stencil = true;
      use-damage = false;
      transparent-clipping = false;

      log-level = "warn";
      log-file = "${XDG_CACHE_DIR/.cache}/picom.log";
      show-all-xerrors = true;

      wintypes = {
        tooltip = {
          fade = true;
          shadow = true;
          opacity = 0.75;
          focus = false;
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
          opacity = 0.8;
        };

        dropdown_menu = {
          opacity = 0.8;
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
