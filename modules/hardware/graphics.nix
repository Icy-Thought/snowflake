{ config, options, lib, pkgs, ... }:
let
  inherit (lib.attrsets) attrValues;
  inherit (lib.modules) mkIf mkMerge;
  cfg = config.modules.hardware.graphics;
in {
  options.modules.hardware.graphics = let inherit (lib.options) mkEnableOption;
  in { amd.enable = mkEnableOption "AMD graphics drivers"; };

  config = mkMerge [
    (mkIf cfg.amd.enable {
      services.xserver.videoDrivers = [ "amdgpu" ];

      hardware.opengl = {
        enable = true;
        driSupport = true;
        extraPackages = attrValues {
          inherit (pkgs) amdvlk vaapiVdpau libvdpau-va-gl vdpauinfo;
          inherit (pkgs.rocmPackages) clr;
          inherit (pkgs.rocmPackages.clr) icd;
        };
        extraPackages32 =
          attrValues { inherit (pkgs.driversi686Linux) amdvlk; };
        driSupport32Bit = true;
      };

      user.packages = attrValues {
        inherit (pkgs) clinfo radeontop radeon-profile;
        inherit (pkgs.rocmPackages)
          rocminfo rocm-smi rocm-runtime rocm-cmake rocm-thunk;
      };
    })
  ];
}
