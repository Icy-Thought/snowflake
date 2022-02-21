{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.shell;
  configDir = config.snowflake.configDir;
in {
  options.modules.shell.git = { enable = mkBoolOpt false; };

  config = mkIf cfg.git.enable (mkMerge [
    {
      user.packages = with pkgs; [
        gitAndTools.gh
        gitAndTools.git-open
        gitAndTools.diff-so-fancy
        git-filter-repo
        (mkIf config.modules.shell.gnupg.enable gitAndTools.git-crypt)
        act
      ];

      homeManager.programs.git = {
        enable = true;
        package = pkgs.gitFull;

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
          signByDefault = true;
          key = "B593E438DDAB3C66";
        };

        extraConfig = {
          init.defaultBranch = "main";
          core = {
            editor = "emacsclient -t";
            pager = "diff-so-fancy | less --tabs=4 -RFX";
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
            "lisp".xfuncname = ''
              "^(((;;;+ )|\\(|([ \t]+\\(((cl-|el-patch-)?def(un|var|macro|method|custom)|gb/))).*)$"
            '';
            "org".xfuncname = ''
              "^(\\*+ +.*)$"
            '';
          };
        };
      };
    }

    (mkIf cfg.fish.enable {
      home.configFile."fish/conf.d/git.fish".source =
        "${configDir}/fish/git.fish";
    })
  ]);
}
