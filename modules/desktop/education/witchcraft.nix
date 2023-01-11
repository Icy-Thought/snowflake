{ options
, config
, lib
, pkgs
, ...
}:

let
  inherit (lib) mkIf;
  inherit (lib.my) mkBoolOpt;
in
{
  options.modules.desktop.education.witchcraft = {
    enable = mkBoolOpt false;
  };

  config = mkIf config.modules.desktop.education.witchcraft.enable {
    # TODO: OSS packages + configuration.
    user.packages = with pkgs; [ kicad ];
  };
}
