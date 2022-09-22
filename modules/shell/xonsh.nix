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

  # Xonsh-Contrib Packages
  xonsh-direnv = pkgs.fetchFromGitHub {
    owner = "74th";
    repo = "xonsh-direnv";
    rev = "1.6.1";
    sha256 = "979y+jUKZkdIyXx4q0f92jX/crFr9LDrA/5hfXm1CpU=";
  };
  xontrib-cmd-durations = pkgs.fetchFromGitHub {
    owner = "jnoortheen";
    repo = "xontrib-cmd-durations";
    rev = "v0.2.9";
    sha256 = "WOhSRENIYFLXiCajlXbwoLzChRRHrwpRKlK4FvyubYo=";
  };
  xontrib-fzf-widgets = pkgs.fetchFromGitHub {
    owner = "laloch";
    repo = "xontrib-fzf-widgets";
    rev = "v0.0.4";
    sha256 = "lz0oiQSLCIQbnoQUi+NJwX82SbUvXJ+3dEsSbOb20q4=";
  };
  xontrib-hist-navigator = pkgs.fetchFromGitHub {
    owner = "jnoortheen";
    repo = "xontrib-hist-navigator";
    rev = "v0.6.4";
    sha256 = "gxiBEp2HUeLW+alqeirRT2wvEI9f9SaA3X7pb3jTQRo=";
  };
  xontrib-output-search = pkgs.fetchFromGitHub {
    owner = "anki-code";
    repo = "xontrib-output-search";
    rev = "0.6.2";
    sha256 = "Nrp5Sj36PVIYi8TPNZ2HrLanW9+JgaxH6/9Lzv0PxGI=";
  };
  xontrib-readable-traceback = pkgs.fetchFromGitHub {
    owner = "vaaaaanquish";
    repo = "xontrib-readable-traceback";
    rev = "0.4.0";
    sha256 = "ek+GTWGUpm2b6lBw/7n4W46W2R0Gy6JxqWoLuQilCXQ=";
  };
  xontrib-schedule = pkgs.fetchFromGitHub {
    owner = "AstraLuma";
    repo = "xontrib-schedule";
    rev = "02c4d4f237451fce39b45545d69ab9ea67565b45";
    sha256 = "Ahog47+Sq9xv03s0kPRm2wEOT989qI3GMaAzlnz/wS4=";
  };

  xontribs = [
    xonsh-direnv
    xontrib-cmd-durations
    xontrib-fzf-widgets
    xontrib-hist-navigator
    xontrib-output-search
    xontrib-readable-traceback
    xontrib-schedule
  ];
in {
  options.modules.shell.xonsh = {
    enable = mkBoolOpt true;
  };

  config = mkIf cfg.enable {
    modules.shell.usefulPkgs.enable = true;

    # Custom shell modules:
    modules.shell.macchina.enable = true;
    modules.shell.xplr.enable = true;
    modules.shell.starship.enable = true;

    programs.xonsh = {
      enable = true;
      package = pkgs.xonsh;
    };

    home.configFile."xonsh/rc.xsh".text = let
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
      $ENABLE_ASYNC_PROMPT = True
      $XONSH_HISTORY_SIZE = "10000 commands"
      $XONSH_STORE_STDOUT = True
      __xonsh__.env['XONTRIB_OUTPUT_SEARCH_KEY'] = 'i'

      #$TITLE = '{current_job:{} | }{user}@{hostname}: {cwd}'

      # -------===[ Core Plugins ]===------- #
      xontrib load abbrevs
      xontrib load bashisms

      # -------===[ 3rd Party Plugins ]===------- #
      import sys
      sys.path.extend(${builtins.toJSON xontribs})
      xontrib load cmd_done direnv fzf-widgets hist_navigator output_search readable-traceback schedule

      # -------===[ Aliases & Abbreviations ]===------- #
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
}
