{ options, config, lib, pkgs, ... }:

with lib; {
  options.modules.develop.cc = {
    enable = mkEnableOption "C/C++ development environment";
  };

  config = mkIf config.modules.develop.cc.enable (mkMerge [
    { user.packages = with pkgs; [ clang clang-tools gdb gnumake xmake ]; }

    (mkIf config.modules.develop.xdg.enable {
      # TODO:
    })
  ]);
}
