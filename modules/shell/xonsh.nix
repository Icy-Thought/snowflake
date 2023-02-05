{ config, options, lib, pkgs, ... }:

let
  inherit (builtins) toJSON;
  inherit (lib) mapAttrsToList mkIf;
  inherit (lib.strings) concatStrings escapeNixString;

  xonsh-direnv = pkgs.fetchFromGitHub {
    owner = "74th";
    repo = "xonsh-direnv";
    rev = "1.6.1";
    hash = "sha256-979y+jUKZkdIyXx4q0f92jX/crFr9LDrA/5hfXm1CpU=";
  };
  xontrib-cmd-durations = pkgs.fetchFromGitHub {
    owner = "jnoortheen";
    repo = "xontrib-cmd-durations";
    rev = "v0.2.9";
    hash = "sha256-WOhSRENIYFLXiCajlXbwoLzChRRHrwpRKlK4FvyubYo=";
  };
  xontrib-fzf-widgets = pkgs.fetchFromGitHub {
    owner = "laloch";
    repo = "xontrib-fzf-widgets";
    rev = "v0.0.4";
    hash = "sha256-lz0oiQSLCIQbnoQUi+NJwX82SbUvXJ+3dEsSbOb20q4=";
  };
  xontrib-hist-navigator = pkgs.fetchFromGitHub {
    owner = "jnoortheen";
    repo = "xontrib-hist-navigator";
    rev = "v0.6.4";
    hash = "sha256-gxiBEp2HUeLW+alqeirRT2wvEI9f9SaA3X7pb3jTQRo=";
  };
  xontrib-output-search = pkgs.fetchFromGitHub {
    owner = "anki-code";
    repo = "xontrib-output-search";
    rev = "0.6.2";
    hash = "sha256-Nrp5Sj36PVIYi8TPNZ2HrLanW9+JgaxH6/9Lzv0PxGI=";
  };
  xontrib-readable-traceback = pkgs.fetchFromGitHub {
    owner = "vaaaaanquish";
    repo = "xontrib-readable-traceback";
    rev = "0.4.0";
    hash = "sha256-ek+GTWGUpm2b6lBw/7n4W46W2R0Gy6JxqWoLuQilCXQ=";
  };
  xontrib-schedule = pkgs.fetchFromGitHub {
    owner = "AstraLuma";
    repo = "xontrib-schedule";
    rev = "02c4d4f237451fce39b45545d69ab9ea67565b45";
    hash = "sha256-Ahog47+Sq9xv03s0kPRm2wEOT989qI3GMaAzlnz/wS4=";
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
  config = mkIf (config.modules.shell.default == "xonsh") {
    modules.shell.usefulPkgs.enable = true;

    # Custom shell modules:
    modules.shell.macchina.enable = true;
    modules.shell.xplr.enable = true;
    modules.shell.starship.enable = true;

    programs.xonsh = {
      enable = true;
      package = pkgs.xonsh;
    };

    home.configFile.xonsh-init = {
      target = "xonsh/rc.xsh";
      text = let abbrevs = import "${config.snowflake.configDir}/shell-abbr";
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
        sys.path.extend(${toJSON xontribs})
        xontrib load cmd_done direnv fzf-widgets hist_navigator output_search readable-traceback schedule

        # -------===[ Aliases & Abbreviations ]===------- #
        aliases[exa] = "exa --group-directories-first"
        aliases[less] = "less -R"

        ${concatStrings (mapAttrsToList (k: v: ''
          abbrevs[${escapeNixString k}] = ${escapeNixString v}
        '') abbrevs)}

        # -------===[ Executing 3rd-Plugins ]===------- #
        execx($(any-nix-shell --info-right))
        execx($(zoxide init xonsh), 'exec', __xonsh__.ctx, filename='zoxide')
        execx($(starship init xonsh))
      '';
    };
  };
}
