{ config, lib, pkgs, ... }: {

  services.picom = {
    enable = true;
    package = pkgs.picom-jonaburg;

    backend = "glx";
    experimentalBackends = true;

    extraOptions = ''
      ### Corners
      corner-radius = 12.0;
      rounded-corners-exclude = [
        "class_g = 'Polybar'",
      ];

      round-borders = 1;
      round-borders-exclude = [ ];

      ### Shadows
      shadow = false;
      shadow-radius = 12;
      shadow-opacity = 0.75;
      shadow-offset-x = -12;
      shadow-offset-y = -12;

      shadow-color = "#000000";

      shadow-exclude = [
        "name = 'Notification'",
        "class_g = 'Dunst'",
        "class_g = 'slop'",
        "class_g = 'Rofi'",
        "_GTK_FRAME_EXTENTS@:c",
        "_NET_WM_STATE@:32a *= '_NET_WM_STATE_HIDDEN'"
      ];

      ### Fading
      fading = true;
      fade-in-step = 0.08;
      fade-out-step = 0.08;
      fade-delta = 2;

      fade-exclude = [];

      no-fading-openclose = false;
      no-fading-destroyed-argb = true;

      ### Transparency / Opacity
      inactive-opacity = 1;
      frame-opacity = 1;
      inactive-opacity-override = false;
      active-opacity = 1.0;
      inactive-dim = 0.0;

      focus-exclude = [
        "class_g ?= 'rofi'",
        "class_g ?= 'slop'",
        "class_g ?= 'Steam'"
      ];

      opacity-rule = [ ]

      ### BG-Blur
      blur: {
        method = "kawase";
        strength = 5.0;
      }

      blur-background = false;
      blur-background-frame = true;
      blur-background-fixed = true;

      blur-background-exclude = [
        "class_g = 'slop'",
        "_GTK_FRAME_EXTENTS@:c"
      ];

      ### General
      daemon = false;
      backend = "glx";
      vsync = true;
      dbus = false;
      mark-wmwin-focused = true;
      mark-ovredir-focused = true;
      detect-rounded-corners = true;
      detect-client-opacity = true;
      refresh-rate = 0;

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

      wintypes: {
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

    '';
  };

}
