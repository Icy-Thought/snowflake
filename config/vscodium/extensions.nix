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
    ms-toolsai.jupyter
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
    haskell.haskell
    justusadam.language-haskell # syntax-highlighting
    james-yu.latex-workshop
    jnoortheen.nix-ide
    ms-python.python
    ms-vscode.cpptools
    rust-lang.rust-analyzer
    tamasfe.even-better-toml
    yzhang.markdown-all-in-one
  ])
  ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
    (import "${themesDir}/${active}/config/vscodium/extension")
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
  ]
