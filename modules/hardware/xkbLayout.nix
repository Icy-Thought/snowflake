{ inputs, options, config, lib, pkgs, ... }:

let cfg = config.modules.hardware.xkbLayout;
in with lib; {
  options.modules.hardware.xkbLayout = {
    hyperCtrl.enable = mkEnableOption "hyper xkb-layout";
  };

  config = mkMerge [
    (mkIf cfg.hyperCtrl.enable {
      services.xserver = {
        displayManager.sessionCommands = ''
          ${getExe pkgs.xorg.setxkbmap} -layout us-hyperCtrl
        '';

        extraLayouts.us-hyperCtrl = {
          description = "US Layout with Right Ctrl = Hyper";
          languages = [ "eng" ];
          symbolsFile = pkgs.writeText "us-hyperCtrl" ''
            partial alphanumeric_keys

            partial modifier_keys
            xkb_symbols "hyperCtrl" {
                include "us(basic)"

                replace key <RCTL> { [ Hyper_R ] };
                modifier_map Mod3 { <RCTL>, <HYPR> };
            };
          '';
        };
      };

      create.dataFile.fcitx5-hyprCtrl =
        mkIf config.modules.desktop.extensions.fcitx5.enable {
          target = "fcitx5/inputmethod/keyboard-us-hyprCtrl.conf";
          text = ''
            [InputMethod]
            Name=us-hyperCtrl
            Icon=input-keyboard
            LangCode=us-hctrl
            Addon=keyboard
            Configurable=True
            Label=us-hctrl
          '';
        };
    })
  ];
}
