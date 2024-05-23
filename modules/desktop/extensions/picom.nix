{
  options,
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib.modules) mkIf mkMerge;
  cfg = config.modules.desktop.extensions.picom;
in {
  options.modules.desktop.extensions.picom = let
    inherit (lib.options) mkEnableOption;
  in {
    enable = mkEnableOption "lightweight X11 compositor";
    animation.enable = mkEnableOption "animated picom";
  };

  config = mkMerge [
    {
      hm.services.picom = {
        enable = true;
        vSync = true;
        settings = {
          inactive-dim = 0.2;
          focus-exclude = ["class_g ?= 'rofi'" "class_g ?= 'Steam'"];
          rounded-corners-exclude = [
            "! name~=''" # Qtile == empty wm_class..
            "window_type = 'dock'"
            "window_type = 'desktop'"
          ];

          corner-radius = 10;
          round-borders = 0;
          round-borders-exclude = [
            "! name~=''" # Qtile == empty wm_class..
          ];

          blur-background = true;
          blur-strength = 10.0;
          blur-method = "dual_kawase";
          blur-backround-exclude = ["window_type != 'dock'"];

          daemon = false;
          dbus = false;
          mark-wmwin-focused = false;
          mark-ovredir-focused = false;
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
        };
      };
    }

    (mkIf (cfg.enable && !cfg.animation.enable) {
      services.picom = {backend = "egl";};
    })

    (mkIf cfg.animation.enable {
      hm.services.picom = {
        package = pkgs.picom.overrideAttrs (_: {
          version = "11.2-unstable";
          src = pkgs.sources.picom;
        });

        backend = "glx";
        extraArgs = [];

        settings = {
          # https://react-spring-visualizer.com/
          animations = true;
          animation-clamping = false;
          animation-dampening = 18;
          animation-stiffness = 200;
          animation-window-mass = 1;
          animation-for-open-window = "slide-up";
          animation-for-unmap-window = "slide-down";

          wintypes = {
            dock = {animation = "slide-down";};
            toolbar = {animation = "slide-down";};
          };
        };
      };
    })
  ];
}
