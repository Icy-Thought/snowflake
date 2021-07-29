{ config, pkgs, ... }: {

  xdg.configFile."leftwm/themes/garden/rofi/icons" = {
    source = ./icons;
    recursive = true;
  };

  xdg.configFile."leftwm/themes/garden/rofi/scripts" = {
    source = ./scripts;
    recursive = true;
  };

  xdg.configFile."leftwm/themes/garden/rofi/launcher.rasi".source =
    ./launcher.rasi;

  xdg.configFile."leftwm/themes/garden/rofi/list-launcher.rasi".source =
    ./list-launcher.rasi;

  xdg.configFile."leftwm/themes/garden/rofi/powermenu.rasi".source =
    ./powermenu.rasi;

  programs.rofi = {
    enable = true;
    extraConfig = {
      font = "JetBrainsMonoMedium Nerd Font Medium 10";
      sidebar-mode = true;
      show-icons = true;
      sorting-method = "fzf";
      fullscreen = false;
      threads = 0;
      matching = "fuzzy";
      scroll-method = 0;
    };
    theme = {
      "*" = {
        transparent = "#00000000";
        foreground = "#F2F2F2EE";
        background-selected = "#F2F2F245";
        background-active = "#F2F2F230";
        background-white = "#F2F2F211";
        background-black = "#00000066";
        urgent = "#E91E6366";
        urgent-selected = "#E91E6377";
      };

      "#window" = {
        transparency = "real";
        background-color = "@transparent";
        text-color = "@foreground";
        location = "southwest";
        anchor = "southwest";
        x-offset = 8;
        y-offset = "-60";
        height = "80%";
        width = "600px";
        orientation = "vertical";
      };

      "#prompt" = { enabled = false; };

      "#button" = {
        action = "ok";
        str = " ";
        font = "FantasqueSansMono Nerd Font 16";
        expand = false;
        text-color = "@foreground";
        background-color = "@transparent";
        vertical-align = "0.5";
        horizontal-align = "0.5";
      };

      "#entry" = {
        font = "JetBrainsMonoMedium Nerd Font Medium 12";
        background-color = "@transparent";
        text-color = "@foreground";
        expand = true;
        vertical-align = "0.5";
        horizontal-align = 0;
        placeholder = "Global Search";
        placeholder-color = "@foreground";
        blink = true;
      };

      "#entry-wrapper" = {
        orientation = "horizontal";
        margin = "0 12px 0 12px";
        spacing = "24px";
        vertical-align = "0.5";
        background-color = "@transparent";
        children = "[ button, entry ]";
      };

      "#inputbar" = {
        padding = "14px";
        margin = "10px 10px 14px 10px";
        background-color = "@background-white";
        text-color = "@foreground";
        expand = false;
        border-radius = "9px";
        position = "north";
        children = "[ entry-wrapper ]";
      };

      "#listview" = {
        background-color = "@transparent";
        spacing = 0;
        cycle = true;
        dynamic = true;
        scrollbar = true;
      };

      "#mainbox" = {
        width = "200px";
        expand = true;
        spacing = "12px";
        padding = "5px";
        background-color = "@background-black";
        children = "[ inputbar, listview ]";
      };

      "#scrollbar" = {
        background-color = "@background-white";
        handle-width = 0;
        margin = "0 0 5px 0";
        border-radius = "9px";
      };

      "#element" = {
        background-color = "@transparent";
        text-color = "@foreground";
        orientation = "horizontal";
        border = 0;
        border-color = "@background-white";
        border-radius = "6px";
        spacing = "24px";
        margin = "0px 12px 0px 12px";
        padding = "10px 24px 10px 24px";
      };

      "#element-icon" = {
        size = "24px";
        border = 0;
        border-color = "@transparent";
      };

      "#element-text" = {
        font = "JetBrainsMonoMedium Nerd Font Medium 11";
        expand = true;
        horizontal-align = 0;
        vertical-align = "0.5";
      };

      "#element normal.urgent,\nelement alternate.urgent" = {
        background-color = "@urgent";
        text-color = "@foreground";
        border-radius = "9px";
      };

      "#element normal.active,\nelement alternate.active" = {
        background-color = "@background-active";
        text-color = "@foreground";
      };

      "#element selected" = {
        background-color = "@background-selected";
        text-color = "@foreground";
      };

      "#element selected.urgent" = {
        background-color = "@urgent-selected";
        text-color = "@foreground";
      };

      "#element selected.active" = {
        background-color = "@background-active";
        color = "@foreground-selected";
      };
    };
  };
}
