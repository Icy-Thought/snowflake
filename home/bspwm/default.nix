{ config, lib, pkgs, ... }:

{
  imports = [ ./picom ./dunst ./rofi ];

  xsession.windowManager.bspwm = {
    enable = true;
    extraConfig = ''
      export GTK_THEME=onedarkish
      export GTK_RC_FILES=~/.config/gtk-2.0
      export GTK_ICON_THEME="WhiteSur-dark"
      exec $BSPWM/autostart.sh &

      # workspaces
      bspc monitor -d 1 2 3 4 5 6 7
      bspc config focus_follows_pointer true
      bspc config top_padding 0
      bspc config normal_border_color  "#282c34"
      bspc config focused_border_color "#abb2bf"
      bspc config urgent_border_color  "#e06c75"
      bspc config presel_border_color  "#b6bdca"


      # borders
      #bspc config focused_border_color "#abb2bf"
      #bspc config normal_border_color "#565c64"
      bspc config border_width 1
      bspc config borderless_monocle true

      #gaps
      bspc config gapless_monocle false
      bspc config window_gap 10
      bspc config split_ratio 0.50

      # desktop 1 mail
      bspc rule -a Geary state=floating desktop=^1

      # desktop 2 web
      # bspc rule -a Firefox state=floating desktop=^2
      # bspc rule -a Brave-Browser state=floating desktop=^2

      # desktop 5 chat
      bspc rule -a Discord state=tiled desktop=^5 follow=on
      bspc rule -a Element state=tiled desktop=^5 follow=on
      bspc rule -a TelegramDesktop state=tiled desktop=^5 follow=on

      # desktop 6 reading
      bspc rule -a Zathura state=floating desktop=^6

      # desktop 7 droplets / virtualization
      bspc rule -a virt-manager state=floating desktop=^7
      bspc rule -a Virt-manager state=floating desktop=^7


      bspc rule -a nitrogen state=floating focus=on
      bspc rule -a feh state=floating focus=on
      bspc rule -a Lxappearance state=floating focus=on
      bspc rule -a thunar state=floating hidden=on sticky=on
      bspc rule -a dropdown state=floating hidden=on sticky=on
      bspc rule -a ranger state=floating hidden=on sticky=on
      bspc rule -a geary-alarm-notify state=floating sticky=on
      alacritty --class dropdown &
      thunar &
          '';
  };
}
