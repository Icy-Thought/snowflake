{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop;
in {
  config = mkIf config.services.xserver.enable {
    assertions = [
      {
        assertion = (countAttrs (n: v: n == "enable" && value) cfg) < 2;
        message = "Can't enable DE/WM <2 at the same time.";
      }
      {
        assertion = let srv = config.services;
        in srv.xserver.enable || srv.sway.enable || !(anyAttrs
          (n: v: isAttrs v && anyAttrs (n: v: isAttrs v && v.enable)) cfg);
        message = "Can't enable a desktop-app without a DE/WM.";
      }
    ];

    user.packages = with pkgs; [ bitwarden qalculate-gtk ];

    # Take care of the garbage:
    system.userActivationScripts.cleanupHome = ''
      pushd "${config.user.home}"
      rm -rf .compose-cache .nv .pki .dbus .fehbg
      [ -s .xsession-errors ] || rm -f .xsession-errors*
      popd
    '';
  };
}
