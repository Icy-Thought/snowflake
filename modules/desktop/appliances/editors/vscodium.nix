{ config, options, lib, pkgs, inputs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.appliances.editors.vscodium;
  acs = config.modules.themes.active;
  themesDir = "${config.snowflake.themesDir}/${acs}";
  codeDir = "${config.snowflake.configDir}/vscodium";
in {
  options.modules.desktop.appliances.editors.vscodium = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    homeManager.programs.vscode = {
      enable = true;
      package = pkgs.vscodium;
      mutableExtensionsDir = true;
      extensions = (with pkgs.vscode-extensions; [
        jock.svg
        vscodevim.vim
        naumovs.color-highlight
        gruntfuggly.todo-tree
        eamodio.gitlens
        mhutchie.git-graph
        ms-toolsai.jupyter
        editorconfig.editorconfig
        formulahendry.code-runner
        christian-kohler.path-intellisense
        github.copilot
        wix.vscode-import-cost
        esbenp.prettier-vscode
        ms-vscode.cpptools
        haskell.haskell
        james-yu.latex-workshop
        yzhang.markdown-all-in-one
        jnoortheen.nix-ide
        ms-python.python
        matklad.rust-analyzer
        tamasfe.even-better-toml
      ]) ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
        (import "${themesDir}/config/vscodium/${acs}")
        # {
        #   name = "dart-code";
        #   publisher = "dart-code";
        #   version = "3.39.20220408";
        #   sha256 = "D7Pjde4MZDHbYel9YfXowEdy5UNS367Mhg3YHH3ty7c=";
        # }
        # {
        #   name = "flutter";
        #   publisher = "dart-code";
        #   version = "3.39.20220405";
        #   sha256 = "OqwzWO8Z/Ql4Y99ki/7/tMFQpltcU5W1cnQc3Wicq+s=";
        # }
        {
          name = "vscode-icons";
          publisher = "vscode-icons-team";
          version = "11.10.0";
          sha256 = "0n96jdmqqh2v7mni4qv08qjxyhp8h82ck9rhmwnxp66ni5ybmj63";
        }
      ];

      userSettings = let
        themeCfg = builtins.fromJSON
          (builtins.readFile "${themesDir}/config/vscodium/themeCfg.json");
        vscCfg =
          builtins.fromJSON (builtins.readFile "${codeDir}/settings.json");
      in [ themeCfg vscCfg ];
      keybindings =
        builtins.fromJSON (builtins.readFile "${codeDir}/keybindings.json");
    };
  };
}
