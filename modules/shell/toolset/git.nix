{ config
, options
, lib
, pkgs
, ...
}:
with lib;
with lib.my; {
  options.modules.shell.git = {
    enable = mkBoolOpt false;
  };

  config = mkIf config.modules.shell.git.enable {
    user.packages = with pkgs; [
      act
      dura
      gitui
      gitAndTools.gh
      gitAndTools.git-open
      (mkIf (config.modules.shell.gnupg.enable) gitAndTools.git-crypt)
    ];

    # Prevent x11 askPass prompt on git push:
    programs.ssh.askPassword = "";

    hm.programs.zsh.initExtra = ''
      # -------===[ Helpful Git Fn's ]===------- #
      gitignore() {
        curl -s -o .gitignore https://gitignore.io/api/$1
      }
    '';

    hm.programs.fish.functions = {
      gitignore = "curl -sL https://www.gitignore.io/api/$argv";
    };

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

      userName = "Icy-Thought";
      userEmail = "icy-thought@pm.me";
      signing = {
        key = "B593E438DDAB3C66";
        signByDefault = true;
      };

      extraConfig = {
        init.defaultBranch = "main";
        core = {
          editor = "nvim";
          whitespace = "trailing-space,space-before-tab";
        };

        tag.gpgSign = true;
        pull.rebase = true;
        push = {
          default = "current";
          gpgSign = "if-asked";
          autoSquash = true;
        };

        github.user = "Icy-Thought";
        gitlab.user = "Icy-Thought";

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
          "lisp".xfuncname = "^(((;;;+ )|\\(|([ 	]+\\(((cl-|el-patch-)?def(un|var|macro|method|custom)|gb/))).*)$";
          "org".xfuncname = "^(\\*+ +.*)$";
        };

        credential.helper = "${pkgs.gitFull}/bin/git-credential-libsecret";
      };
    };
  };
}
