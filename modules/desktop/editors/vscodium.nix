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
  cfg = config.modules.desktop.editors.vscodium;
  vscDir = "${config.snowflake.configDir}/vscodium";
in {
  options.modules.desktop.editors.vscodium = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    hm.programs.vscode = with config.snowflake; {
      enable = true;
      package = pkgs.vscodium;
      mutableExtensionsDir = true;

      # Config imports
      extensions =
        pkgs.vscode-utils.extensionsFromVscodeMarketplace (
          (import "${vscDir}/custom-extensions.nix").extensions
        )
        ++ [
          # Editor
          pkgs.vscode-extensions.eamodio.gitlens
          pkgs.vscode-extensions.editorconfig.editorconfig
          pkgs.vscode-extensions.mhutchie.git-graph
          pkgs.vscode-extensions.vscodevim.vim

          # Aesthetics
          pkgs.vscode-extensions.esbenp.prettier-vscode
          pkgs.vscode-extensions.gruntfuggly.todo-tree
          pkgs.vscode-extensions.jock.svg
          pkgs.vscode-extensions.naumovs.color-highlight

          # Toolset
          pkgs.vscode-extensions.christian-kohler.path-intellisense
          pkgs.vscode-extensions.formulahendry.code-runner
          pkgs.vscode-extensions.github.copilot
          pkgs.vscode-extensions.wix.vscode-import-cost

          # Language specific
          pkgs.vscode-extensions.james-yu.latex-workshop
          pkgs.vscode-extensions.tamasfe.even-better-toml
          pkgs.vscode-extensions.yzhang.markdown-all-in-one
        ];

      userSettings = import "${vscDir}/settings.nix" {inherit config;};
      keybindings = import "${vscDir}/keybindings.nix" {};
    };
  };
}
