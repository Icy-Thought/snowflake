{
  config,
  pkgs,
  ...
}: let
  active = config.modules.themes.active;
  themesDir = config.snowflake.themesDir;
in
  (with pkgs.vscode-extensions; [
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
  ])
  ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
    (import "${themesDir}/${active}/config/vscodium/extension")
    {
      name = "vscode-icons";
      publisher = "vscode-icons-team";
      version = "11.12.0";
      sha256 = "jyc6m6lfwSYt5xg2H5TGdxheKptho87C7eN4xuU5IYg=";
    }
  ]
