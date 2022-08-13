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
    ereshkigal.enable = mkBoolOpt false; # fnl
    agasaya.enable = mkBoolOpt false; # lua
  };

  config = mkMerge [
    {
      nixpkgs.overlays = with inputs; [nvim-nightly.overlay];

      user.packages = with pkgs; (mkMerge [
        [neovide]
        (mkIf (!config.modules.develop.cc.enable) [gcc]) # Treesitter
      ]);

      hm.programs.neovim = {
        enable = true;
        package = pkgs.neovim-nightly;
        withRuby = true;
        withPython3 = true;
        withNodeJs = true;
      };

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
