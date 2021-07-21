{ config, lib, pkgs, ... }: {

  services.picom = {
    enable = true;
    backend = "glx";
    experimentalBackends = true;

    inactiveOpacity = 1.0;
    opacityRules = [];
    menuOpacity = 0.90;

    fade = true;
    fadeDelta = 2;
    fadeExclude = [];
    fadeSteps = [0.08 0.08];

    refreshRate = 0;

    shadow = false;
    shadowExclude = [
      "name = 'Notification'"
      "class_g = 'Dunst'"
      "class_g = 'slop'"
      "class_g = 'Rofi'"
      "_GTK_FRAME_EXTENTS@:c"
      "_NET_WM_STATE@:32a *= '_NET_WM_STATE_HIDDEN'"
    ];

    shadowOffsets = [ (-12) (-12) ];
    shadowOpacity = 0.75;

    vSync = true;

    settings = {
      ### Corners ###
      corner-radius = 12.0;
      round-borders = 1;
      round-borders-exclude = [ ];
      rounded-corners-exclude = [ "class_g = 'Polybar'" ];

      ### Shadows ###
      shadow-radius = 12;
      shadow-color = "#000000";

      ### Fading ###
      no-fading-openclose = false;
      no-fading-destroyed-argb = true;

      ### Transparency / Opacity ###
      frame-opacity = 1;
      inactive-opacity-override = false;

      focus-exclude = [
        "class_g ?= 'rofi'"
        "class_g ?= 'slop'"
        "class_g ?= 'Steam'"
      ];

      ### Background-Blur ###
      blur = {
        method = "kawase";
        strength = "5.0";
      };

      blur-background = false;
      blur-background-frame = true;
      blur-background-fixed = true;

      blur-background-exclude = [
      	"class_g = 'slop'"
      	"_GTK_FRAME_EXTENTS@:c"
      ];

      ### General ###
      daemon = false;
      dbus = false;
      mark-wmwin-focused = true;
      mark-ovredir-focused = true;
      detect-rounded-corners = true;
      detect-client-opacity = true;

      unredir-if-possible-exclude = [];
      detect-transient = true;
      detect-client-leader = true;
      resize-damage = 1;

      invert-color-include = [];
      glx-no-stencil = true;
      use-damage = true;
      transparent-clipping = false;

      log-level = "warn";
      log-file = "~/.cache/picom-log.log";
      show-all-xerrors = true;

      wintypes = {
      	tooltip = { fade = true; focus = false; };
      	normal = {  };
      	dock = {  };
      	popup_menu = { focus = false; opacity = 0.90; };
      	dropdown_menu = { focus = false; };
      	above = {  };
      	splash = {  };
      	utility = { focus = false; blur-background = false; };
      	notification = {  };
      	desktop = { blur-background = false; };
      	menu = { focus = false; };
      	dialog = {  };
      };
    };
  };
}
