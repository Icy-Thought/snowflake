{ options, config, lib, pkgs, ... }:

let
  inherit (builtins) readFile;
  inherit (lib.attrsets) attrValues optionalAttrs;
  inherit (lib.modules) mkIf;
in {
  options.modules.shell.toolset.git = let inherit (lib.options) mkEnableOption;
  in { enable = mkEnableOption "version-control system"; };

  config = mkIf config.modules.shell.toolset.git.enable {
    user.packages = attrValues ({
      inherit (pkgs) act dura lazygit;
      inherit (pkgs.gitAndTools) gh git-open;
    } // optionalAttrs config.modules.shell.toolset.gnupg.enable {
      inherit (pkgs.gitAndTools) git-crypt;
    });

    # Prevent x11 askPass prompt on git push:
    programs.ssh.askPassword = "";

    hm.programs.zsh.initExtra = ''
      # -------===[ Helpful Git Fn's ]===------- #
      gitignore() {
        curl -sLw "\n" https://www.toptal.com/developers/gitignore/api/$@ -o .gitignore
      ;}
    '';

    hm.programs.fish.functions.gitignore =
      "curl -sL https://www.toptal.com/developers/gitignore/api/$argv -o .gitignore";
    home.sessionVariables.GITHUB_TOKEN = "$(cat /run/agenix/tokenGH)";

    hm.programs.git = {
      enable = true;
      package = pkgs.gitFull;
      difftastic = {
        enable = true;
        background = "dark";
        color = "always";
        display = "inline";
      };

      aliases = {
        unadd = "reset HEAD";

        # Data Analysis:
        ranked-authors = "!git authors | sort | uniq -c | sort -n";
        emails = ''
          !git log --format="%aE" | sort -u
        '';
        email-domains = ''
          !git log --format="%aE" | awk -F'@' '{print $2}' | sort -u
        '';
      };

      attributes = [ "*.lisp diff=lisp" "*.el diff=lisp" "*.org diff=org" ];

      ignores = [
        # General:
        "*.bloop"
        "*.bsp"
        "*.metals"
        "*.metals.sbt"
        "*metals.sbt"
        "*.direnv"
        "*.envrc"
        "*hie.yaml"
        "*.mill-version"
        "*.jvmopts"

        # Emacs:
        "*~"
        "*.*~"
        "\\#*"
        ".\\#*"

        # OS-related:
        ".DS_Store?"
        ".DS_Store"
        ".CFUserTextEncoding"
        ".Trash"
        ".Xauthority"
        "thumbs.db"
        "Thumbs.db"
        "Icon?"

        # Compiled residues:
        "*.class"
        "*.exe"
        "*.o"
        "*.pyc"
        "*.elc"
      ];

      extraConfig = {
        init.defaultBranch = "main";
        core = {
          editor = "nvim";
          whitespace = "trailing-space,space-before-tab";
        };
        credential.helper = "${pkgs.gitFull}/bin/git-credential-libsecret";

        user = {
          name = "Icy-Thought";
          email = "icy-thought@pm.me";
          signingKey = readFile "${config.user.home}/.ssh/id_ed25519.pub";
        };

        gpg.format = "ssh";
        commit.gpgSign = true;
        tag.gpgSign = true;

        push = {
          default = "current";
          gpgSign = "if-asked";
          autoSquash = true;
        };
        pull.rebase = true;

        filter = {
          required = true;
          smudge = "git-lfs smudge -- %f";
          process = "git-lfs filter-process";
          clean = "git-lfs clean -- %f";
        };

        url = {
          "https://github.com/".insteadOf = "gh:";
          "git@github.com:".insteadOf = "ssh+gh:";
          "git@github.com:icy-thought/".insteadOf = "gh:/";
          "https://gitlab.com/".insteadOf = "gl:";
          "https://gist.github.com/".insteadOf = "gist:";
          "https://bitbucket.org/".insteadOf = "bb:";
        };

        diff = {
          "lisp".xfuncname =
            "^(((;;;+ )|\\(|([  ]+\\(((cl-|el-patch-)?def(un|var|macro|method|custom)|gb/))).*)$";
          "org".xfuncname = "^(\\*+ +.*)$";
        };
      };
    };
  };
}
