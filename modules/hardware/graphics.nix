{ options, config, lib, pkgs, ... }:

let cfg = config.modules.hardware.graphics;
in with lib; {
  options.modules.hardware.graphics = {
    amd.enable = mkEnableOption "AMD graphics drivers";
  };

  config = mkMerge [
    (mkIf cfg.amd.enable {
      services.xserver.videoDrivers = [ "amdgpu" ];

      hardware.opengl = {
        enable = true;
        driSupport = true;
        extraPackages = with pkgs; [
          amdvlk
          vaapiVdpau
          libvdpau-va-gl
          vdpauinfo
          rocmPackages.clr
          rocmPackages.clr.icd
        ];
        extraPackages32 = [ pkgs.driversi686Linux.amdvlk ];
        driSupport32Bit = true;
      };

      user.packages = with pkgs;
        [ clinfo radeontop radeon-profile ] ++ (with rocmPackages; [
          rocminfo
          rocm-smi
          rocm-runtime
          rocm-cmake
          rocm-thunk
        ]);
    })
  ];
}
