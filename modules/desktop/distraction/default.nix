{ config
, options
, lib
, pkgs
, ...
}:
with lib;
with lib.my;

let cfg = config.modules.desktop.distraction.hardware;
in {
  options.modules.desktop.distraction = {
    hardware = {
      amdvlk.enable = mkBoolOpt false;
    };
  };

  config = mkMerge [
    (mkIf cfg.amdvlk.enable {
      hardware.opengl = {
        enable = true;
        driSupport = true;
        extraPackages = with pkgs; [
          rocm-opencl-icd
          rocm-opencl-runtime
        ];
        driSupport32Bit = true;
      };

      environment.variables.VK_ICD_FILENAMES = [
        "/run/opengl-driver/share/vulkan/icd.d/amd_icd64.json"
      ];
    })
  ];
}
