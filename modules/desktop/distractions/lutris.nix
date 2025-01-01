{ options, config, lib, pkgs, ... }:

let cfg = config.modules.desktop.distractions.lutris;
in with lib; {
  options.modules.desktop.distractions.lutris = {
    enable = mkEnableOption "libre game-manager";
    league.enable = mkEnableOption "lutris LoL setup";
  };

  config = mkMerge [
    (mkIf cfg.enable {
      user.packages = with pkgs;
        [ (lutris.override { extraLibraries = pkgs: [ pkgs.jansson ]; }) ]
        ++ optionals (config.modules.virtualisation.wine.enable == false) [
          winetricks
          wineWowPackages.fonts
          wineWowPackages.stagingFull
        ];
    })

    (mkIf (cfg.enable && cfg.league.enable) {
      boot.kernel.sysctl."abi.vsyscall32" = 0; # anti-cheat...
      networking.firewall.allowedTCPPorts = [ 443 ];
      environment.variables.QT_X11_NO_MITSHM = "1";

      user.packages = with pkgs; [ vulkan-tools dxvk gnome.zenity ];
    })
  ];
}
