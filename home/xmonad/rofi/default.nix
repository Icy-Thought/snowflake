{ config, pkgs, ... }: {

  services.picom.enable = true;

  programs.rofi = {
    enable = true;
    extraConfig = {
      modi = "windw,run,ssh,drun";
      font = "JetBrainsMono Nerd Font 12";
      icon-theme = "Whitesur-dark";
      drun-use-desktop-cache = true;
      drun-reload-desktop-cache = true;
      display-window = "";
      display-drun = "";
      display-combi = " Search";
    };

    theme = {
      "*" = {
        background-color = "#282C33";
        border-color = "#2e343f";
        text-color = "#8ca0aa";
        spacing = 0;
      };

      "#inputbar" = {
        border = "0 0 1px 0";
        children = "[prompt,entry,num-filtered-rows,textbox-num-sep,num-rows]";
      };

      "#prompt" = {
        padding = "16px";
        border = "0 1px 0 0";
      };

      "#textbox" = {
        background-color = "#2e343f";
        border = "0 0 1px 0";
        border-color = "#282C33";
        padding = "8px 16px";
      };

      "#entry" = {
        padding = "16px";
        blink = true;
        placeholder-color = "#5a6372";
        placeholder = "Type to filer";
      };

      "#listview" = {
        cycle = false;
        margin = "0 0 -1px 0";
        scrollbar = false;
      };

      "#element" = {
        border = "0 0 1px 0";
        padding = "16px";
      };

      "#element-icon" = { size = "24px"; };
      "#element-text" = { vertical-align = "0.5"; };
      "#element selected" = { background-color = "#2e343f"; };

      "#num-filtered-rows" = {
        vertical-align = "0.5";
        expand = false;
        text-color = "#5a6372";
      };

      "#num-rows" = {
        padding = "0px 16px 0px 0px";
        vertical-align = "0.5";
        expand = false;
        text-color = "#5a6372";
      };

      "#textbox-num-sep" = {
        vertical-align = "0.5";
        expand = false;
        str = "/";
        text-color = "#5a6372";
      };
    };
  };
}
