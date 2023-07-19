{
  options,
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib.meta) getExe;
  inherit (lib.modules) mkIf;
  inherit (pkgs) pamixer python3 writeScriptBin;
in {
  options.modules.shell.scripts.volume = let
    inherit (lib.options) mkEnableOption;
  in {enable = mkEnableOption "volume control";};

  config = mkIf config.modules.shell.scripts.volume.enable {
    user.packages = [
      pamixer

      (writeScriptBin "volctl" ''
        #!${getExe python3}

        import argparse
        import subprocess
        import time

        parser = argparse.ArgumentParser(
            prog="Audio Volume Control",
            description="A simple script to help control the volume levels of the audio transmitted by your hardware device(s)!",
            epilog="Please, try again.",
        )

        subparser = parser.add_subparsers(dest="command")

        increase = subparser.add_parser("increase")
        decrease = subparser.add_parser("decrease")
        mute = subparser.add_parser("toggle-mute")
        status = subparser.add_parser("status")

        increase.add_argument(
            "-l", "--level",
            type=int,
            default=5,
            help="Increase audio volume levels by +x%. (default: +%(default)%)",
        )

        decrease.add_argument(
            "-l", "--level",
            type=int,
            default=5,
            help="Decrease audio volume levels by -x%. (default: -%(default)%)",
        )

        mute.add_argument(
            "-t", "--toggle-mute",
            action="store_true",
            help="Toggle mute for device audio system. (default: False)",
        )


        args = parser.parse_args()

        mute_status = subprocess.run(
            ["pamixer", "--get-mute"],
            stdout=subprocess.PIPE,
            universal_newlines=True,
        )
        get_mute = mute_status.stdout.strip()

        get_volume = subprocess.run(
            ["pamixer", "--get-volume"],
            stdout=subprocess.PIPE,
            universal_newlines=True,
        )

        if get_volume.returncode == 0:
            volume_level = get_volume.stdout.strip()
        else:
            volume_level = "5"

        percentage = int(volume_level)

        LEVEL = [" ", " ", "墳 ", " "]

        if args.command == "toggle-mute" and get_mute == "false":
            ICON = LEVEL[0]
        elif percentage <= 30:
            ICON = LEVEL[1]
        elif 30 < percentage <= 70:
            ICON = LEVEL[2]
        elif percentage > 70:
            ICON = LEVEL[3]
        else:
            ICON = ""


        def notification():
            if args.command == "toggle-mute" and get_mute == "false":
                volume = "0"
                message = ICON + "Muted Output Audio!"
            else:
                volume = volume_level
                message = ICON + "Volume Level: " + volume + "%"

            return subprocess.run(
                [
                    "notify-send", message,
                    "-t", "500",
                    "-h", "string:synchronous:volume",
                    "-h", "int:value:" + volume,
                    "-u", "low",
                ]
            )


        def pamixer(param: str, lvl: str):
            subprocess.run(["pamixer", param, lvl])


        def change_volume_level():
            if args.command == "toggle-mute" and get_mute == "false":
                command = pamixer("-t", "")
            elif args.command == "toggle-mute" and get_mute == "true":
                command = pamixer("-t", "")
            elif args.command != "toggle-mute" and get_mute == "true":
                command = pamixer("-t", "")
            elif args.command == "increase":
                command = pamixer("-i", str(args.level))
            elif args.command == "decrease":
                command = pamixer("-d", str(args.level))
            else:
                return

            return command, notification()


        change_volume_level()


        def volume_status():
            if args.command == "status":
                while True:
                    print(ICON + volume_level + "%")
                    time.sleep(60)
            else:
                return


        volume_status()
      '')
    ];
  };
}
