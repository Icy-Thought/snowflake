{
  options,
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib.meta) getExe;
  inherit (lib.modules) mkIf mkMerge;
  cfg = config.modules.desktop.distraction.steam;
in {
  options.modules.desktop.distraction.steam = let
    inherit (lib.options) mkEnableOption;
    inherit (lib.types) str;
    inherit (lib.my) mkOpt;
  in {
    enable = mkEnableOption "game/software store";
    hardware.enable = mkEnableOption "Steam-based HW support";
    libDir = mkOpt str "$XDG_DATA_HOME/steamlib";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      # I avoid programs.steam.enable because it installs another steam binary,
      # which the xdesktop package invokes, instead of my steam shims below.
      user.packages = let
        inherit
          (pkgs)
          makeDesktopItem
          stdenv
          steam
          steam-run-native
          writeScriptBin
          ;
      in [
        # Get steam to keep its garbage out of $HOME
        (writeScriptBin "steam" ''
          #!${stdenv.shell}
          HOME="${cfg.libDir}" exec ${getExe steam} "$@"
        '')

        # for running GOG and humble bundle games
        (writeScriptBin "steam-run" ''
          #!${stdenv.shell}
          HOME="${cfg.libDir}" exec ${getExe steam-run-native} "$@"
        '')

        # Add rofi desktop icon
        (makeDesktopItem {
          name = "steam";
          desktopName = "Steam";
          icon = "steam";
          exec = "steam";
          terminal = false;
          mimeTypes = ["x-scheme-handler/steam"];
          categories = ["Network" "FileTransfer" "Game"];
        })
      ];

      system.userActivationScripts.setupSteamDir = ''
        mkdir -p "${cfg.libDir}"
      '';

      # better for steam proton games
      systemd.extraConfig = "DefaultLimitNOFILE=1048576";
    }

    (mkIf cfg.hardware.enable {hardware.steam-hardware.enable = true;})
  ]);
}
