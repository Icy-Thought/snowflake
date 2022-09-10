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
  activeTheme = config.modules.themes.editor.neovim;
in {
  options.modules.desktop.editors.neovim = {
    ereshkigal.enable = mkBoolOpt false; # fnl
    agasaya.enable = mkBoolOpt false; # lua
  };

  config = mkMerge [
    {
      nixpkgs.overlays = [inputs.nvim-nightly.overlay];

      user.packages = with pkgs; [
        neovim-nightly
        neovide
        (mkIf (!config.modules.develop.cc.enable) gcc) # Treesitter
      ];

      environment.shellAliases = {
        vi = "nvim";
        vim = "nvim";
        vdiff = "nvim -d";
      };

      environment.variables = {
        SQLITE_PATH = "${pkgs.sqlite.out}/lib/libsqlite3.so";
      };
    }

    (mkIf cfg.agasaya.enable {
      modules.develop.lua.enable = true;

      hm.programs.neovim.extraConfig = ''
        luafile ${builtins.toString nvimDir + "/agasaya/init.lua"}
      '';
    })

    (mkIf cfg.ereshkigal.enable {
      modules.develop.lua.fennel.enable = true;

      hm.programs.neovim.extraConfig = ''
        luafile ${builtins.toString nvimDir + "/ereshkigal/init.lua"}
      '';
    })
  ];
}
