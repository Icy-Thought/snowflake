{ config, options, lib, pkgs, ... }:

let
  inherit (builtins) toJSON;
  inherit (lib.attrsets) attrValues mapAttrsToList;
  inherit (lib.modules) mkIf;
  inherit (lib.strings) concatStrings escapeNixString;

  xontribs = attrValues {
    inherit (pkgs.sources)
      xonsh-direnv xontrib-cmd-durations xontrib-fzf-widgets
      xontrib-hist-navigator xontrib-output-search xontrib-readable-traceback
      xontrib-schedule;
  };
in {
  config = mkIf (config.modules.shell.default == "xonsh") {
    modules.shell = {
      corePkgs.enable = true;
      toolset.starship.enable = true;
    };

    programs.xonsh = {
      enable = true;
      package = pkgs.xonsh;
    };

    create.configFile.xonsh-init = {
      target = "xonsh/rc.xsh";
      text =
        let abbrevs = import "${config.snowflake.configDir}/shell-abbr.nix";
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

          # -------===[ Main Plugins ]===------- #
          xontrib load abbrevs
          xontrib load bashisms

          # -------===[ 3rd Party Plugins ]===------- #
          import sys
          sys.path.extend(${toJSON xontribs})
          xontrib load cmd_done direnv fzf-widgets hist_navigator output_search readable-traceback schedule

          # -------===[ Aliases & Abbreviations ]===------- #
          aliases[eza] = "eza --group-directories-first"
          aliases[less] = "less -R"

          ${concatStrings (mapAttrsToList (k: v: ''
            abbrevs[${escapeNixString k}] = ${escapeNixString v}
          '') abbrevs)}


          # -------===[ Useful Functions ]===------- #
          def sysdate():
              subprocess.run(['nixos-rebuild', 'switch', '--use-remote-sudo', '--flake', '.' + str(subprocess.run(['hostname'], stdout=subprocess.PIPE).stdout.decode().strip()), '--impure'])

          # -------===[ Executing 3rd-Plugins ]===------- #
          execx($(any-nix-shell xonsh))
          execx($(zoxide init xonsh), 'exec', __xonsh__.ctx, filename='zoxide')
          execx($(starship init xonsh))
        '';
    };
  };
}
