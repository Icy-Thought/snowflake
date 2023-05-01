{ config, options, lib, pkgs, ... }:

let
  inherit (lib.attrsets) attrValues;
  inherit (lib.modules) mkIf mkMerge;
  cfg = config.modules.desktop.distraction.hardware;
in {
  options.modules.desktop.distraction =
    let inherit (lib.options) mkEnableOption;
    in {
      hardware = { amdvlk.enable = mkEnableOption "AMD open-source driver"; };
    };

  config = mkMerge [
    (mkIf cfg.amdvlk.enable {
      hardware.opengl = {
        enable = true;
        driSupport = true;
        extraPackages =
          attrValues ({ inherit (pkgs) rocm-opencl-icd rocm-opencl-runtime; });
        driSupport32Bit = true;
      };

      environment.variables.VK_ICD_FILENAMES =
        [ "/run/opengl-driver/share/vulkan/icd.d/amd_icd64.json" ];
    })
  ];
}
