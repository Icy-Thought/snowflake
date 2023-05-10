{ config, options, lib, pkgs, ... }:

let
  inherit (lib.modules) mkIf;
  inherit (lib.strings) optionalString;
  cfg = config.modules.shell.xplr;
in {
  options.modules.shell.xplr = let inherit (lib.options) mkEnableOption;
  in {
    enable = mkEnableOption "TUI file-explorer";
    fennel.enable = mkEnableOption "lisp-configured xplr";
  };

  config = mkIf cfg.enable {
    user.packages = [ pkgs.xplr ];

    home.configFile.xplr-init = {
      target = "xplr/init.lua";
      text = ''
        ---@diagnostic disable
        version = "0.20.0"
        local xplr = xplr
        ---@diagnostic enable

        -- Lua search path
        local home = os.getenv("HOME")
        package.path = home
            .. "/.config/xplr/plugins/?/init.lua;"
            .. home
            .. "/.config/xplr/plugins/?.lua;"
            .. package.path

        -- XPM (init)
        local xpm_path = home .. "/.local/share/xplr/dtomvan/xpm.xplr"
        local xpm_url = "https://github.com/dtomvan/xpm.xplr"

        package.path = package.path
            .. ";"
            .. xpm_path
            .. "/?.lua;"
            .. xpm_path
            .. "/?/init.lua"

        os.execute(
            string.format(
                "[ -e '%s' ] || git clone '%s' '%s'",
                xpm_path,
                xpm_url,
                xpm_path
            )
        )

        -- plugin-related
        xplr.config.modes.builtin.default.key_bindings.on_key.x = {
            help = "xpm",
            messages = {
                "PopMode",
                { SwitchModeCustom = "xpm" },
            },
        }

        require("xpm").setup({
            auto_cleanup = true,
            auto_install = true,
            plugins = {
                "dtomvan/xpm.xplr",
                { name = "sayanarijit/comex.xplr" },
                { name = "sayanarijit/preview-tabbed.xplr" },
                { name = "sayanarijit/map.xplr" },
                { name = "sayanarijit/find.xplr" },
                { name = "sayanarijit/xargs.xplr" },
                { name = "sayanarijit/nvim-ctrl.xplr" },
                { name = "sayanarijit/zoxide.xplr" },
            },
        })

        -- initializing plugins
        require("map").setup()

        -- personalized configurations
        xplr.config.general.enable_mouse = true
        xplr.config.general.show_hidden = true
        xplr.config.general.enable_recover_mode = true

        ${optionalString (cfg.fennel.enable) ''
          -- add support for fennel
          local fennel = require("fennel")

          fennel.path = fennel.path
              .. ";"
              .. home
              .. "/.config/xplr/plugins/?/init.fnl;"
              .. home
              .. "/.config/xplr/plugins/?.fnl;"

          table.insert(package.loaders or package.searchers, fennel.searcher)
        ''}
      '';
    };
  };
}
