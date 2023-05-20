{
  config,
  options,
  lib,
  pkgs,
  inputs,
  ...
}: let
  inherit (lib) mkIf mkEnableOption;

  vscDir = "${config.snowflake.configDir}/vscodium";
in {
  options.modules.desktop.editors.vscodium = {
    enable = mkEnableOption "telemetry-free vscode";
  };

  config = mkIf config.modules.desktop.editors.vscodium.enable {
    hm.programs.vscode = {
      enable = true;
      package = pkgs.vscodium;
      mutableExtensionsDir = true;

      # Config imports
      extensions =
        pkgs.vscode-utils.extensionsFromVscodeMarketplace
        ((import "${vscDir}/custom-extensions.nix").extensions)
        ++ (with pkgs.vscode-extensions; [
          # Editor
          eamodio.gitlens
          editorconfig.editorconfig
          mhutchie.git-graph
          vscodevim.vim

          # Aesthetics
          esbenp.prettier-vscode
          gruntfuggly.todo-tree
          jock.svg
          naumovs.color-highlight

          # Toolset
          christian-kohler.path-intellisense
          formulahendry.code-runner
          github.copilot
          wix.vscode-import-cost

          # Language specific
          james-yu.latex-workshop
          tamasfe.even-better-toml
          yzhang.markdown-all-in-one
        ]);
      userSettings = import "${vscDir}/settings.nix" {inherit config;};
      keybindings = import "${vscDir}/keybindings.nix" {};
    };
  };
}
