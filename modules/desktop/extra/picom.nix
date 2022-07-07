{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.extra.picom;
in {
  options.modules.desktop.extra.picom = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    services.picom = {
      enable = true;
      backend = "glx";
      experimentalBackends = true;
      vSync = true;

      fade = true;
      fadeDelta = 2;
      fadeSteps = [2.5e-2 2.5e-2];

      menuOpacity = 0.9;
      inactiveOpacity = 0.65;
      opacityRules = [
        "100:window_type = 'combo'"
        "100:window_type = 'desktop'"
        "100:window_type = 'dialog'"
        "100:window_type = 'dnd'"
        "100:window_type = 'dock'"
        "100:window_type = 'dropdown_menu'"
        "100:window_type = 'menu'"
        "100:window_type = 'normal'"
        "100:window_type = 'notification'"
        "100:window_type = 'popup_menu'"
        "100:window_type = 'splash'"
        "100:window_type = 'toolbar'"
        "100:window_type = 'unknown'"
        "100:window_type = 'utility'"
      ];

      shadow = false;
      shadowOffsets = [(-7) (-7)];
      shadowOpacity = 0.75;

      shadowExclude = [
        "! name~=''" # Qtile == empty wm_class..
        "_COMPTON_SHADOW@:32c = 0"
        "_GTK_FRAME_EXTENTS@:c"
        "class_g = 'firefox-aurora' && window_type = 'popup_menu'"
        "class_g = 'firefox-aurora' && window_type = 'tooltip'"
        "class_g = 'firefox-aurora' && window_type = 'utility'"
        "class_g = 'Rofi'"
        "class_g = 'slop'" # Maim
        "window_type = 'desktop'"
        "window_type = 'dnd'"
        "window_type = 'dock'"
        "window_type = 'notification'"
        "window_type = 'toolbar'"
        "window_type = 'utility'"
      ];

      settings = {
        shadow-radius = 10;

        corner-radius = 10;
        rounded-corners-exclude = [
          "! name~=''" # Qtile == empty wm_class..
          "window_type = 'dock'"
          "window_type = 'desktop'"
        ];
        round-borders = 0;
        round-borders-exclude = [
          "! name~=''" # Qtile == empty wm_class..
        ];

        blur-background = false;
        blur-background-frame = false;
        blur-background-fixed = false;

        # WARN: Official Picom animations (unsupported as of 06/07/22)
        # transition-length = 200;
        # transition-pow-x = 5.0e-2;
        # transition-pow-y = 5.0e-2;
        # transition-pow-w = 5.0e-2;
        # transition-pow-h = 5.0e-2;
        # size-transition = true;

        # WARN: Unofficial animation support (dccsillag)
        animations = true;
        animation-stiffness = 170;
        animation-window-mass = 0.8;
        animation-dampening = 15;
        animation-clamping = false;
        animation-for-open-window = "zoom";
        animation-for-unmap-window = "zoom";
        animation-for-transient-window = "slide-up";

        daemon = false;
        dbus = false;
        mark-wmwin-focused = true;
        mark-ovredir-focused = true;
        detect-rounded-corners = true;
        detect-client-opacity = true;

        unredir-if-possible = false;
        unredir-if-possible-exclude = [];
        detect-transient = true;
        detect-client-leader = true;

        invert-color-include = [];
        glx-no-stencil = true;
        use-damage = false;
        transparent-clipping = false;
        inactive-opacity-override = false;

        focus-exclude = ["class_g ?= 'rofi'" "class_g ?= 'Steam'"];

        # WARN: Unofficial animation support (dccsillag)
        wintypes = {
          dock = {animation = "slide-down";};
          toolbar = {animation = "slide-down";};
        };
      };
    };
  };
}
