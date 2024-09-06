{ config, options, lib, pkgs, ... }:
let inherit (lib.modules) mkIf;
in {
  options.modules.shell.toolset.lsd = let inherit (lib.options) mkEnableOption;
  in { enable = mkEnableOption "Next-gen ls command"; };

  config = mkIf config.modules.shell.toolset.btop.enable {
    hm.programs.lsd = {
      enable = true;
      enableAliases = false;
      settings = {
        blocks = [ "permission" "size" "user" "group" "date" "name" ];
        date = "relative";
        # ignore-globs = [".git" ".hg"];
        header = true;
        sorting = {
          column = "name";
          dir-grouping = "first";
        };
      };

      colors = let inherit (config.modules.themes) active;
      in (mkIf (active != null) {
        user = "cyan";
        group = "yellow";
        date = {
          hour-old = "grey";
          day-old = "grey";
          older = "dark_grey";
        };
        inode = {
          valid = "blue";
          invalid = "dark_grey";
        };
        links = {
          valid = "blue";
          invalid = "dark_grey";
        };
        permission = {
          read = "green";
          write = "yellow";
          exec = "red";
          exec-sticky = "magenta";
          no-access = "dark_grey";
          octal = "cyan";
          acl = "cyan";
          context = "cyan";
        };
        size = {
          none = "grey";
          small = "green";
          medium = "yellow";
          large = "red";
        };
        tree-edge = "dark_grey";
      });
    };
  };
}
