{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.appliances.media.docViewer;
in {
  options.modules.desktop.appliances.media.docViewer = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable { user.packages = with pkgs; [ zathura ]; };
}
