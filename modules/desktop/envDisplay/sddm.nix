{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.envDisplay.sddm;
in {
  options.modules.envDisplay.sddm = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    imports = [ "${config.snowflake.dir}/packages/sddm-themes.nix" ];
    services.xserver.displayManager.sddm.enable = true;
  };
}
