{ options, config, lib, pkgs, ... }:

with lib; {
  options.modules.virtualisation.looking-glass = {
    enable = mkEnableOption "KVM(s) VGA PCI Pass-through without peripherals.";
  };

  config = mkIf config.modules.virtualisation.looking-glass.enable {

    hm.programs.looking-glass-client = {
      enable = true;
      package = pkgs.unstable.looking-glass-client;
      settings = {
        app = {
          allowDMA = true;
          shmFile = "/dev/shm/looking-glass";
        };
        input = {
          rawMouse = true;
          escapeKey = "56"; # linux/input-event-codes.h
        };
        spice = {
          enable = true;
          audio = true;
        };
        win = {
          autoResize = true;
          borderless = true;
          quickSplash = true;
        };
      };
    };
  };
}
