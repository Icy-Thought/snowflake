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
      nixpkgs.overlays = with inputs; [neovim-nightly.overlay];

      user.packages = with pkgs; (mkMerge [
        [
          neovide
          neovim-nightly
          (python3.withPackages (ps: with ps; [pynvim]))
        ]
        (mkIf (!config.modules.develop.cc.enable) [gcc]) # Treesitter
      ]);

      environment.shellAliases = {
        vi = "nvim";
        vim = "nvim";
        vimdiff = "nvim -d";
      };
    }

    (mkIf cfg.agasaya.enable {
      modules.develop.lua.enable = true;

      environment.variables = {
        SQLITE_PATH = "${pkgs.sqlite.out}/lib/libsqlite3.so";
        # MYVIMRC = "${nvimDir}/agasaya/init.lua";
      };
    })

    (mkIf cfg.ereshkigal.enable {
      modules.develop.lua.fennel.enable = true;

      home.configFile."nvim" = {
        source = "${nvimDir}/ereshkigal";
        recursive = true;
      };
    })
  ];
}
