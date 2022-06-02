{config, ...}: let
  cfg = config.modules.themes;
in {
  # Introduction
  "__comment" = "(VSCode) settings.json => immutable conf-file, managed via (Nix) Home-Manager";
  "update.mode" = "none";
  "workbench.settings.editor" = "ui";
  "files.exclude" = {
    "**/.classpath" = true;
    "**/.factorypath" = true;
    "**/.project" = true;
    "**/.settings" = true;
    "**/*.exe" = true;
    "**/*.o" = true;
  };

  # Colorscheme
  "workbench.colorTheme" = "${cfg.vscode.theme.dark}";
  "workbench.preferredDarkColorTheme" = "${cfg.vscode.theme.dark}";
  "workbench.preferredLightColorTheme" = "${cfg.vscode.theme.light}";

  # Font-related
  "editor.fontFamily" = "${cfg.font.sans.family}";
  "editor.fontWeight" = "${cfg.font.sans.weightNum}";
  "editor.fontSize" = 17;

  "debug.console.fontFamily" = "${cfg.font.mono.family}";
  "debug.console.fontWeight" = "${cfg.font.mono.weightNum}";
  "debug.console.fontSize" = 16;

  "terminal.integrated.fontFamily" = "${cfg.font.mono.family}";
  "terminal.integrated.fontWeight" = "${cfg.font.mono.weightNum}";
  "terminal.integrated.fontSize" = 16;

  # Aesthetics
  "window.menuBarVisibility" = "toggle";
  "breadcrumbs.enabled" = true;
  "workbench.iconTheme" = "vscode-icons";
  "workbench.tree.indent" = 8;
  "editor.scrollbar.vertical" = "auto";
  "editor.roundedSelection" = true;
  "editor.smoothScrolling" = true;

  # Terminal
  "terminal.integrated.env.linux" = {};
  "terminal.explorerKind" = "integrated";
  "terminal.external.linuxExec" = "kitty";
  "terminal.integrated.tabs.focusMode" = "singleClick";
  "terminal.integrated.copyOnSelection" = true;
  "terminal.integrated.scrollback" = 5000;
  "terminal.integrated.persistentSessionScrollback" = 200;
  "terminal.integrated.mouseWheelScrollSensitivity" = 2;

  # Explorer
  "explorer.sortOrder" = "default";
  "explorer.sortOrderLexicographicOptions" = "default";
  "explorer.incrementalNaming" = "smart";

  # Search
  "search.smartCase" = true;
  "search.followSymlinks" = false;
  "search.sortOrder" = "default";
  "search.searchEditor.doubleClickBehaviour" = "goToLocation";

  # Editor
  "files.autoSave" = "onWindowChange";
  "editor.formatOnSave" = true;
  "editor.bracketPairColorization.enabled" = true;
  "editor.guides.bracketPairs" = "active";
  "editor.tabSize" = 4;
  "editor.mouseWheelScrollSensitivity" = 2;
  "editor.quickSuggestionsDelay" = 10;
  "zenMode.hideLineNumbers" = false;
  "markdown.preview.typographer" = true;
  "markdown.extension.print.theme" = "dark";

  # Language specific
  "haskell.formattingProvider" = "stylish-haskell";

  "[nix]" = {"editor.tabSize" = 2;};

  # Git
  "git.autoFetch" = false;
  "git.enableCommitSigning" = true;
  "git.defaultCloneDirectory" = "~/git";
  "git.promptToSaveFilesBeforeCommit" = "staged";
  "gitlens.defaultDateFormat" = "H:mm:ss dd.MM.yy";
  "gitlens.statusBar.alignment" = "left";
  "gitlens.views.repositories.location" = "scm";

  # Vim
  "vim.easymotion" = true;
  "vim.incsearch" = true;
  "vim.useSystemClipboard" = true;
  "vim.useCtrlKeys" = true;
  "vim.hlsearch" = true;
  "vim.insertModeKeyBindings" = [
    {
      "before" = ["j" "j"];
      "after" = ["<Esc>"];
    }
  ];
  "vim.normalModeKeyBindingsNonRecursive" = [
    {
      "before" = ["<leader>" "d"];
      "after" = ["d" "d"];
    }
    {
      "before" = ["<C-n>"];
      "commands" = [" =nohl"];
    }
    {
      "before" = ["K"];
      "commands" = ["lineBreakInsert"];
      "silent" = true;
    }
  ];
  "vim.leader" = "<space>";
  "vim.handleKeys" = {
    "<C-a>" = false;
    "<C-f>" = false;
  };
}
