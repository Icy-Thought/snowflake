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
  options.modules.shell.scripts.microphone = let
    inherit (lib.options) mkEnableOption;
  in {enable = mkEnableOption "microphone control";};

  config = mkIf config.modules.shell.scripts.microphone.enable {
    user.packages = [
      pamixer

      (writeScriptBin "micvol" ''
        #!${getExe python3}

        import argparse
        import subprocess
        import time

        parser = argparse.ArgumentParser(
            prog="Microphone Volume Control",
            description="A simple script to help control the internal microphone levels of your hardware device(s)!",
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
            help="Increase microphone levels by +x%. (default: +%(default)%)",
        )

        decrease.add_argument(
            "-l", "--level",
            type=int,
            default=5,
            help="Decrease microphone levels by -x%. (default: -%(default)%)",
        )

        mute.add_argument(
            "-t", "--toggle-mute",
            action="store_true",
            help="Toggle mute for device microphone. (default: False)",
        )


        args = parser.parse_args()

        mute_status = subprocess.run(
            ["pamixer", "--default-source", "--get-mute"],
            stdout=subprocess.PIPE,
            universal_newlines=True,
        )
        get_mute = mute_status.stdout.strip()

        get_micVol = subprocess.run(
            ["pamixer", "--default-source", "--get-volume"],
            stdout=subprocess.PIPE,
            universal_newlines=True,
        )
        micVol_level = get_micVol.stdout.strip()


        def notification():
            ICONS = [" ", " "]
            if args.command == "toggle-mute" and get_mute == "false":
                micvol = "0"
                message = ICONS[0] + "Muted Microphone!"
            else:
                micvol = micVol_level
                message = ICONS[1] + "Microphone Volume: " + micVol_level + "%"

            return subprocess.run(
                [
                    "notify-send", message,
                    "-t", "500",
                    "-h", "string:synchronous:volume",
                    "-h", "int:value:" + micvol,
                    "-u", "low",
                ]
            )


        def pamixer(param: str, lvl: str):
            subprocess.run(["pamixer", "--default-source", param, lvl])


        def change_micVol_level():
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


        change_micVol_level()


        def micVol_status():
            if args.command == "status":
                while True:
                    print(" " + micVol_level + "%")
                    time.sleep(60)
            else:
                return


        micVol_status()
      '')
    ];
  };
}
