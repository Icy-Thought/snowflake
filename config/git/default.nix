{ config, pkgs, ... }:

let
  gitConfig = {
    core = {
      editor = "emacsclient -t";
      pager = "diff-so-fancy | less --tabs=4 -RFX";
    };

    init.defaultBranch = "main";
    pull.rebase = false;
  };

in {
  home.packages = with pkgs; [ gist ];

  programs.git = {
    enable = true;

    aliases = {
      amend = "commit --amend -m";
      fixup =
        "!f(){ git reset --soft HEAD~\${1} && git commit --amend -C HEAD; };f";
      br = "branch";
      co = "checkout";
      st = "status";
      ls = ''
        log --pretty=format:"%C(yellow)%h%Cred%d\\ %Creset%s%Cblue\\ [%cn]" --decorate'';
      ll = ''
        log --pretty=format:"%C(yellow)%h%Cred%d\\ %Creset%s%Cblue\\ [%cn]" --decorate --numstat'';
      cm = "commit -m";
      ca = "commit -am";
      dc = "diff --cached";
    };

    extraConfig = gitConfig;
    ignores = [
      "*.bloop"
      "*.bsp"
      "*.metals"
      "*.metals.sbt"
      "*metals.sbt"
      "*.direnv"
      "*.envrc" # there is lorri, nix-direnv & simple direnv; let people decide
      "*hie.yaml" # ghcide files
      "*.mill-version" # used by metals
      "*.jvmopts" # should be local to every project
    ];

    userName = "Icy-Thought";
    userEmail = "gilganyx@pm.me";

    signing = {
      signByDefault = true;
      key = "22CF239720AAD89B";
    };
  };
}
