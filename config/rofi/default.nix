{ config, pkgs, ... }: {

  programs.rofi = {
    enable = true;
    plugins = with pkgs; [ rofi-systemd rofi-emoji rofi-calc ];

    extraConfig = {
      font = "Noto Sans 10";
      show-icons = true;
      icon-theme = "Whitesur-dark";
      terminal = "alacritty";
      display-drun = "λ";
      drun-display-format = "{name}";
      disable-history = false;
      fullscreen = false;
      hide-scrollbar = true;
      sidebar-mode = false;
    };

    theme = let inherit (config.lib.formats.rasi) mkLiteral;
    in {
      "*" = {
        background = mkLiteral "#00000000";
        background-alt = mkLiteral "#00000000";
        background-bar = mkLiteral "#f2f2f215";
        foreground = mkLiteral "#f2f2f2EE";
        accent = mkLiteral "#3DAEE966";
      };

      "window" = {
        transparency = "real";
        background-color = mkLiteral "@background";
        text-color = mkLiteral "@foreground";
        border = mkLiteral "0px";
        border-color = mkLiteral "@border";
        border-radius = mkLiteral "0px";
        width = mkLiteral "40%";
        location = mkLiteral "center";
        x-offset = mkLiteral "0";
        y-offset = mkLiteral "0";
      };

      "prompt" = {
        enabled = mkLiteral "true";
        padding = mkLiteral "0.30% 1% 0% -0.5%";
        background-color = mkLiteral "@background-alt";
        text-color = mkLiteral "@foreground";
        font = "FantasqueSansMono Nerd Font 12";
      };

      "entry" = {
        background-color = mkLiteral "@background-alt";
        text-color = mkLiteral "@foreground";
        placeholder-color = mkLiteral "@foreground";
        expand = mkLiteral "true";
        horizontal-align = mkLiteral "0";
        placeholder = "Search";
        padding = mkLiteral "0.10% 0% 0% 0%";
        blink = mkLiteral "true";
      };

      "inputbar" = {
        children = map mkLiteral [ "prompt" "entry" ];
        background-color = mkLiteral "@background-bar";
        text-color = mkLiteral "@foreground";
        expand = mkLiteral "false";
        border = mkLiteral "0% 0% 0% 0%";
        border-radius = mkLiteral "12px";
        border-color = mkLiteral "@accent";
        margin = mkLiteral "0% 0% 0% 0%";
        padding = mkLiteral "1.5%";
      };

      "listview" = {
        background-color = mkLiteral "@background-alt";
        columns = mkLiteral "5";
        lines = mkLiteral "3";
        spacing = mkLiteral "0%";
        cycle = mkLiteral "false";
        dynamic = mkLiteral "true";
        layout = mkLiteral "vertical";
      };

      "mainbox" = {
        background-color = mkLiteral "@background-alt";
        border = mkLiteral "0% 0% 0% 0%";
        border-radius = mkLiteral "0% 0% 0% 0%";
        border-color = mkLiteral "@accent";
        children = map mkLiteral [ "inputbar" "listview" ];
        spacing = mkLiteral "2%";
        padding = mkLiteral "2% 1% 2% 1%";
      };

      "element" = {
        background-color = mkLiteral "@background-alt";
        text-color = mkLiteral "@foreground";
        orientation = mkLiteral "vertical";
        border-radius = mkLiteral "0%";
        padding = mkLiteral "2% 0% 2% 0%";
      };

      "element-icon" = {
        size = mkLiteral "64px";
        border = mkLiteral "0px";
      };

      "element-text" = {
        expand = mkLiteral "true";
        horizontal-align = mkLiteral "0.5";
        vertical-align = mkLiteral "0.5";
        margin = mkLiteral "0.5% 0.5% -0.5% 0.5%";
      };

      "element selected" = {
        background-color = mkLiteral "@background-bar";
        text-color = mkLiteral "@foreground";
        border = mkLiteral "0% 0% 0% 0%";
        border-radius = mkLiteral "12px";
        border-color = mkLiteral "@accent";
      };
    };
  };

}
