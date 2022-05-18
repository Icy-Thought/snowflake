{
  config,
  options,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.editors.neovim;
  nvimDir = "${config.snowflake.configDir}/nvim.d";
  colorscheme = config.modules.themes.neovim.theme;
in {
  options.modules.desktop.editors.neovim = {
    enable = mkBoolOpt false;
    fnl.enable = mkBoolOpt false;
    lua.enable = mkBoolOpt false;
  };

  config = mkMerge [
    {
      nixpkgs.overlays = [inputs.neovim-nightly.overlay];

      user.packages = with pkgs; [neovim-nightly];

      environment = {
        # variables.NVIMDIR = "${nvimDir}/agasaya";
        shellAliases = {
          vi = "nvim";
          vim = "nvim";
          vimdiff = "nvim -d";
        };
      };
    }

    (mkIf cfg.fnl.enable {
      modules.develop.lua.fennel.enable = true;

      home.configFile."nvim" = {
        source = "${nvimDir}/ereshkigal";
        recursive = true;
      };
    })

    (mkIf cfg.lua.enable {
      modules.develop.lua.enable = true;

      home.configFile."nvim" = {
        source = "${nvimDir}/agasaya";
        recursive = true;
      };

      home.file."./config/nvim/init.lua".text = ''
        require("core.packer")
        require("core.settings")

        require("keymaps.default")
        require("keymaps.which-key")

        -- Apply colorscheme
        require("themes.${colorscheme}")
        vim.cmd("colorscheme ${colorscheme}")
      '';
    })
  ];
}
