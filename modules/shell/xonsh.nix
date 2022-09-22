{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.shell.xonsh;
in {
  options.modules.shell.xonsh = {
    enable = mkBoolOpt true;
  };

  config = mkIf cfg.enable {
    hm.programs.direnv = {
      enable = true;
      nix-direnv.enable = true;
      config.whitelist.prefix = ["/home"];
    };

    # Enable starship-rs:
    modules.shell.starship.enable = true;

    programs.xonsh = {
      enable = true;
      package = pkgs.xonsh;
      config = let
        aliases = {
          exa = "exa --group-directories-first";
        };
        abbrevs = {
          ls = "exa -Slhg --icons";
          lsa = "exa -Slhga --icons";
          tree = "exa -SlhgT --icons";
          emc = "emacsclient -c";
          tmc = "emacsclient -t";
          usbStat = "watch rg -e Dirty: -e Writeback: /proc/meminfo";
          wget = "curl -O";

          # -------===[ Nix ]===------- #
          nb = "nix-build -E 'with import <nixpkgs> {}; callPackage ./. {}'";
          np = "nix-shell -p";
          nls = "nix-store --query --requisites /run/current-system | cut -d- -f2- | sort | uniq";

          # -------===[ Sys-Management ]===------- #
          bat0 = "upower -i /org/freedesktop/UPower/devices/battery_BAT0";
          flkup = "nix flake update";
          thkup = "nixos-rebuild switch --use-remote-sudo --flake .#thinkpad-e595 --impure";
          proup = "nixos-rebuild switch --use-remote-sudo --flake .#probook-440g3 --impure";
          d2nix = "dconf dump / | dconf2nix > dconf.nix";

          # -------===[ Others ]===------- #
          wud = "systemctl stop wg-quick-akkadianVPN.service";
          wup = "systemctl start wg-quick-akkadianVPN.service";
          yta = "youtube-dl -x --audio-format mp3";
          ytv = "youtube-dl --best-quality";
        };
      in ''
        # -------===[ Settings ]===------- #
        $AUTO_CD = True
        $COLOR_RESULTS = True
        $COMPLETIONS_BRACKETS = True
        $XONSH_HISTORY_SIZE = "10000 commands"
        $XONSH_STORE_STDOUT = True
        __xonsh__.env['XONTRIB_OUTPUT_SEARCH_KEY'] = 'i'

        # -------===[ XContrib Plugins ]===------- #
        xontrib load abbrevs
        xontrib load bashisms
        xontrib load direnv
        xontrib load fzf-widgets
        xontrib load histcpy
        xontrib load output_search
        xontrib load readable-traceback
        xontrib load schedule

        ${lib.concatStrings (lib.mapAttrsToList (k: v:
          with lib.strings; ''
            aliases[${escapeNixString k}] = ${escapeNixString v}
          '')
        aliases)}

        ${lib.concatStrings (lib.mapAttrsToList (k: v:
          with lib.strings; ''
            abbrevs[${escapeNixString k}] = ${escapeNixString v}
          '')
        abbrevs)}

        # -------===[ Executing 3rd-Plugins ]===------- #
        ## Zoxide
        execx($(zoxide init xonsh), 'exec', __xonsh__.ctx, filename='zoxide')

        ## Starship-rs:
        execx($(starship init xonsh))
      '';
    };
  };
}
