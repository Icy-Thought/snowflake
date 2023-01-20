{ options
, config
, lib
, pkgs
, ...
}:

let
  inherit (lib) mkIf mkMerge;
  inherit (lib.my) mkBoolOpt;

  cfg = config.modules.desktop.extensions.picom;
in
{
  options.modules.desktop.extensions.picom = {
    enable = mkBoolOpt false;
    animation.enable = mkBoolOpt false;
  };

  config = mkMerge [
    {
      hm.services.picom = {
        enable = true;
        vSync = true;

        shadow = false;
        shadowOffsets = [ (-7) (-7) ];
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
          inactive-dim = 0.2;
          focus-exclude = [ "class_g ?= 'rofi'" "class_g ?= 'Steam'" ];

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

          blur = {
            method = "dual_kawase";
            strength = 10.0;
            background = true;
            background-frame = false;
            background-fixed = false;
            background-exclude = [ "window_type != 'dock'" ];
          };

          daemon = false;
          dbus = false;
          mark-wmwin-focused = false;
          mark-ovredir-focused = false;
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
        };
      };
    }

    (mkIf (cfg.enable && !cfg.animation.enable) {
      services.picom = {
        backend = "egl";
        fade = false;
      };
    })

    (mkIf cfg.animation.enable {
      hm.nixpkgs.overlays = [
        (final: prev: {
          picom = prev.picom.overrideAttrs (old: {
            version = "2022-05-30";
            src = prev.fetchFromGitHub {
              owner = "dccsillag";
              repo = "picom";
              rev = "51b21355696add83f39ccdb8dd82ff5009ba0ae5";
              sha256 = "crCwRJd859DCIC0pEerpDqdX2j8ZrNAzVaSSB3mTPN8=";
            };
          });
        })
      ];

      hm.services.picom = {
        backend = "glx";
        extraArgs = [ "--experimental-backends" ];

        fade = true;
        fadeDelta = 2;
        fadeSteps = [ 2.5e-2 2.5e-2 ];

        settings = {
          animations = true;
          animation-stiffness = 170;
          animation-window-mass = 0.8;
          animation-dampening = 15;
          animation-clamping = false;
          animation-for-open-window = "zoom";
          animation-for-unmap-window = "zoom";
          animation-for-transient-window = "slide-up";

          wintypes = {
            dock = { animation = "slide-down"; };
            toolbar = { animation = "slide-down"; };
          };
        };
      };
    })
  ];
}
