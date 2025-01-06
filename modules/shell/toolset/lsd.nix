{ options, config, lib, pkgs, ... }:

with lib; {
  options.modules.shell.toolset.lsd = {
    enable = mkEnableOption "Next-gen ls command";
  };

  config = mkIf config.modules.shell.toolset.lsd.enable {
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

      colors = mkIf (config.modules.themes.active != null) {
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
      };
    };
  };
}
