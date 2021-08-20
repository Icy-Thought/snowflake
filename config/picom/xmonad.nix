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
    fadeSteps = [ (3.0e-2) (3.0e-2) ];
    fadeExclude = [ "class_g = 'slop'" ];

    refreshRate = 0;

    shadow = true;
    shadowExclude = [
      "_GTK_FRAME_EXTENTS@:c"
      "name = 'hacksaw'"
      "class_g ?= 'taffybar'"
      "class_g = 'Rofi'"
      "class_g = 'Firefox' && window_type = 'utility'"
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
        "window_type = 'menu'"
        "window_type = 'dropdown_menu'"
        "window_type = 'popup_menu'"
        "window_type = 'tooltip'"
        "name = 'rofi - Search'"
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

      unredir-if-possible-exclude = [ ];
      detect-transient = true;
      detect-client-leader = true;

      invert-color-include = [ ];
      glx-no-stencil = true;
      use-damage = false;
      transparent-clipping = false;

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
