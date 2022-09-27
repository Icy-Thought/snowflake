{ inputs
, options
, config
, lib
, pkgs
, ...
}:
with lib;
with lib.my; let
  cfg = config.modules.hardware.xkbLayout;
in
{
  options.modules.hardware.xkbLayout = {
    hyperCtrl.enable = mkBoolOpt false;
  };

  config =
    let
      hyperCLayout = pkgs.writeText "hyper-ctrl-layout" ''
        xkb_keymap {
          xkb_keycodes  { include "evdev+aliases(qwerty)" };
          xkb_types     { include "complete"      };
          xkb_compat    { include "complete"      };

          partial modifier_keys
          xkb_symbols "hyper" {
            include "pc+us+inet(evdev)+terminate(ctrl_alt_bksp)"
            key  <RCTL> { [ Hyper_R, Hyper_R ] };
            modifier_map Mod3 { <HYPR>, Hyper_R };
          };

          xkb_geometry  { include "pc(pc104)"     };
        };
      '';
    in
    mkIf cfg.hyperCtrl.enable {
      hm.xsession.initExtra = ''
        # Set XKB layout = us+hyper on WM start:
        ${getExe pkgs.xorg.xkbcomp} ${hyperCLayout} $DISPLAY
      '';

      environment.etc.hyperKey = {
        target = "X11/keymap.xkb";
        source = hyperCLayout;
      };
    };
}
