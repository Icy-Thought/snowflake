{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.shell;
in {
  options.modules.shell.xplr = {
    enable = mkBoolOpt false;
  };

  config = mkIf (cfg.xplr.enable || cfg.fish.enable) {
    user.packages = with pkgs; [xplr];

    home.configFile."xplr/init.lua".text = ''
      ---@diagnostic disable
      version = "0.19.0"
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
              { name = "sayanarijit/xclip.xplr" },
              { name = "sayanarijit/zoxide.xplr" },
          },
      })

      -- initializing plugins
      require("map").setup()

      -- personalized configurations
      xplr.config.general.enable_mouse = true
      xplr.config.general.show_hidden = true
      xplr.config.general.enable_recover_mode = true

      -- add support for fennel
      local fennel = require("fennel")

      fennel.path = fennel.path
          .. ";"
          .. home
          .. "/.config/xplr/plugins/?/init.fnl;"
          .. home
          .. "/.config/xplr/plugins/?.fnl;"

      table.insert(package.loaders or package.searchers, fennel.searcher)
    '';
  };
}
