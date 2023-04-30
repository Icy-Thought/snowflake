{ options, config, lib, pkgs, ... }:

let
  inherit (lib.meta) getExe;
  inherit (lib.modules) mkIf;
  inherit (pkgs) maim slop python3 writeScriptBin;
in {
  options.modules.shell.scripts.screenshot =
    let inherit (lib.options) mkEnableOption;
    in { enable = mkEnableOption false; };

  config = mkIf config.modules.shell.scripts.screenshot.enable {
    user.packages = [
      maim
      slop

      (writeScriptBin "scrcapy" ''
        #!${getExe python3}

        import argparse
        import subprocess
        from datetime import datetime
        from pathlib import Path

        parser = argparse.ArgumentParser(
            prog="Capture Toolkit",
            description="A simple script to help capture images of your environment(s)!",
            epilog="Please, try again.",
        )

        # Necessary for confusion prevention by Py:
        parent_parser = argparse.ArgumentParser()

        parent_parser.add_argument(
            "-w", "--workspace",
            action="store_true",
            help="Capture image of your active workspace.",
        )

        parent_parser.add_argument(
            "-a", "--active-window",
            action="store_true",
            help="Capture image of the current active window.",
        )

        parent_parser.add_argument(
            "-s", "--selection",
            action="store_true",
            help="Capture image of the highlighted region.",
        )

        subparser = parser.add_subparsers(dest="command")

        system = subparser.add_parser("system", parents=[parent_parser], add_help=False)
        clipboard = subparser.add_parser("clipboard", parents=[parent_parser], add_help=False)

        args = parser.parse_args()


        def notify(image: Path, title: str, message: str):
            return subprocess.run(
                [
                    "notify-send",
                    "-t", "750",
                    "-h", "string:synchronous:screenshot",
                    "-i", image,
                    "-u", "low",
                    title, message
                ]
            )


        if args.workspace:
            TITLE = "Workspace"
            PARAMETER = ""
        elif args.active_window:
            TITLE = "Active-Win"
            PARAMETER = "-i $(xdotool getactivewindow)"
        elif args.selection:
            TITLE = "Selection"
            PARAMETER = "-s"


        def capture_screen():
            CURRENT_DATETIME = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
            FILENAME = TITLE + " " + CURRENT_DATETIME + ".png"

            if args.command == "system":
                filepath = Path.home() / "Pictures" / "Screenshots" / repr(FILENAME)

                maim_cmd = ["maim", "-u", PARAMETER, str(filepath)]
                subprocess.run(" ".join(maim_cmd), shell=True)

                notify(
                    Path.home() / "Pictures" / "Screenshots" / FILENAME,
                    " ".join(["Archived Screenshot!", " "]),
                    CURRENT_DATETIME,
                )
            elif args.command == "clipboard":
                filepath = Path("/tmp") / repr(FILENAME)

                maim_cmd = ["maim", "-u", PARAMETER, str(filepath)]
                subprocess.run(" ".join(maim_cmd), shell=True)

                xclip_cmd = [
                    "xclip",
                    "-selection", "clipboard",
                    "-t", "image/png",
                    "<", str(filepath),
                ]
                subprocess.run(" ".join(xclip_cmd), shell=True)

                notify(
                    Path("/tmp") / FILENAME,
                    " ".join(["Clipped Screenshot!", "⎘"]),
                    CURRENT_DATETIME,
                )
            else:
                return


        capture_screen()
      '')
    ];
  };
}
