{ options, config, lib, pkgs, ... }:

let
  inherit (lib) getExe mkIf;
  inherit (lib.my) mkBoolOpt;
  inherit (pkgs) python3 writeScriptBin;
in {
  options.modules.shell.scripts.brightness = { enable = mkBoolOpt false; };

  config = mkIf config.modules.shell.scripts.brightness.enable {
    # WARNING: won't work unless light -> udev..
    programs.light.enable = true;

    user.packages = [
      (writeScriptBin "brightctl" ''
        #!${getExe python3}

        import argparse
        import subprocess

        parser = argparse.ArgumentParser(
            prog="Brightness Control",
            description="A simple script to help control the brightness levels of your environment!",
            epilog="Please, try again.",
        )

        subparser = parser.add_subparsers(dest="command")

        increase = subparser.add_parser("increase")
        decrease = subparser.add_parser("decrease")

        increase.add_argument(
            "-l",
            "--level",
            type=int,
            default=5,
            help="Increase brightness levels by +x%. (default: +%(default)%)",
        )

        decrease.add_argument(
            "-l",
            "--level",
            type=int,
            default=5,
            help="Decrease brightness levels by -x%. (default: -%(default)%)",
        )


        args = parser.parse_args()

        get_brightness = subprocess.check_output(["light", "-G"], shell=True).strip()
        brightness_level = str(float(get_brightness))


        def notify(message):
            return subprocess.run(
                [
                    "notify-send",
                    message,
                    "-t",
                    "500",
                    "-h",
                    "string:synchronous:brightness",
                    "-h",
                    "int:value:" + brightness_level,
                    "-u",
                    "low",
                ]
            )


        def light(param: str):
            subprocess.run(["light", param, str(args.level)])


        def change_brightness():
            if args.command == "increase":
                ICON = "  "
                light("-A")
            elif args.command == "decrease":
                ICON = "  "
                light("-U")
            else:
                return

            notify(ICON + "Brightness: " + brightness_level + "%")


        change_brightness()
      '')
    ];
  };
}
