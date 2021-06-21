# Generated via dconf2nix: https://github.com/gvolpe/dconf2nix
{ lib, ... }:

let
  mkTuple = lib.hm.gvariant.mkTuple;
in
{
  dconf.settings = {
    "ca/desrt/dconf-editor" = {
      saved-pathbar-path = "/org/gnome/desktop/applications/office/";
      saved-view = "/org/gnome/desktop/applications/office/";
    };

    "com/github/johnfactotum/Foliate/view" = {
      bg-color = "#282828";
      fg-color = "#ebdbb2";
      invert = false;
      layout = "auto";
      link-color = "#83a598";
      prefer-dark-theme = true;
    };

    "io/github/celluloid-player/celluloid" = {
      settings-migrated = true;
    };

    "io/github/celluloid-player/celluloid/window-state" = {
      loop-playlist = false;
      show-controls = true;
      show-playlist = false;
      volume = 1.0;
    };

    "org/gnome/Geary" = {
      ask-open-attachment = true;
      compose-as-html = true;
      formatting-toolbar-visible = false;
      migrated-config = true;
      optional-plugins = [ "sent-sound" ];
      startup-notifications = true;
    };

    "org/gnome/boxes" = {
      first-run = false;
      view = "icon-view";
    };

    "org/gnome/control-center" = {
      last-panel = "keyboard";
    };

    "org/gnome/desktop/applications/terminal" = {
      exec = "alacritty";
    };

    "org/gnome/desktop/calendar" = {
      show-weekdate = true;
    };

    "org/gnome/desktop/input-sources" = {
      mru-sources = [ (mkTuple [ "xkb" "us" ]) (mkTuple [ "xkb" "se" ]) (mkTuple [ "xkb" "ara" ]) ];
      per-window = true;
      sources = [ (mkTuple [ "xkb" "us" ]) (mkTuple [ "xkb" "se" ]) (mkTuple [ "xkb" "ara" ]) (mkTuple [ "ibus" "libpinyin" ]) ];
      xkb-options = [ "lv3:ralt_switch" ];
    };

    "org/gnome/desktop/interface" = {
      clock-show-seconds = true;
      clock-show-weekday = true;
      font-antialiasing = "grayscale";
      font-hinting = "slight";
      gtk-im-module = "ibus";
      monospace-font-name = "JetBrainsMono Nerd Font Medium 12";
      show-battery-percentage = true;
    };

    "org/gnome/desktop/peripherals/keyboard" = {
      numlock-state = true;
    };

    "org/gnome/desktop/peripherals/touchpad" = {
      natural-scroll = true;
      speed = 0.60;
      tap-to-click = true;
      two-finger-scrolling-enabled = true;
    };

    "org/gnome/desktop/privacy" = {
      disable-camera = false;
      disable-microphone = false;
      recent-files-max-age = 30;
      remove-old-temp-files = true;
      remove-old-trash-files = true;
    };

    "org/gnome/desktop/session" = {
      idle-delay = "uint32 300";
    };

    "org/gnome/desktop/sound" = {
      allow-volume-above-100-percent = false;
    };

    "org/gnome/desktop/wm/preferences" = {
      button-layout = "appmenu:minimize,maximize,close";
      workspace-names = "@as []";
    };

    "org/gnome/eog/view" = {
      background-color = "rgb(0,0,0)";
      use-background-color = true;
    };

    "org/gnome/epiphany" = {
      ask-for-default = false;
      default-search-engine = "DuckDuckGo";
    };

    "org/gnome/epiphany/reader" = {
      color-scheme = "dark";
    };

    "org/gnome/epiphany/web" = {
      default-zoom-level = 1.0;
    };

    "org/gnome/file-roller/listing" = {
      list-mode = "as-folder";
      name-column-width = 268;
      show-path = false;
      sort-method = "name";
      sort-type = "ascending";
    };

    "org/gnome/gnome-screenshot" = {
      auto-save-directory = "file:///home/sirius/Pictures/Screenshots";
    };

    "org/gnome/maps" = {
      hybrid-aerial = true;
      map-type = "MapsAerialSource";
      night-mode = true;
      transportation-type = "pedestrian";
      window-maximized = true;
    };

    "org/gnome/mutter" = {
      attach-modal-dialogs = true;
      dynamic-workspaces = true;
      edge-tiling = true;
      focus-change-on-pointer-rest = true;
      workspaces-only-on-primary = true;
    };

    "org/gnome/nautilus/preferences" = {
      default-folder-viewer = "icon-view";
      search-filter-time-type = "last_modified";
      search-view = "list-view";
      show-create-link = false;
    };

    "org/gnome/nautilus/window-state" = {
      maximized = false;
    };

    "org/gnome/settings-daemon/plugins/media-keys" = {
      area-screenshot = [ "@as []" ];
      custom-keybindings = [ "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/" "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom1/" "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom2/" ];
      screenshot = [ "@as []" ];
      window-screenshot = [ "@as []" ];
    };

    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0" = {
      binding = "<Shift>Print";
      command = "gnome-screenshot -a";
      name = "Screenshot Selected Area";
    };

    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom1" = {
      binding = "<Alt>Print";
      command = "gnome-screenshot -w";
      name = "Screenshot Active Application Window";
    };

    "org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom2" = {
      binding = "Print";
      command = "gnome-screenshot";
      name = "Screenshot Display";
    };

    "org/gnome/settings-daemon/plugins/power" = {
      sleep-inactive-ac-type = "nothing";
      sleep-inactive-battery-type = "suspend";
    };

    "org/gnome/shell" = {
      disable-user-extensions = false;
      favorite-apps = [ "org.gnome.Nautilus.desktop" "org.gnome.Fractal.desktop" "discord.desktop" "org.gnome.Geary.desktop" "firefox.desktop" "emacs.desktop" "Alacritty.desktop" "qalculate-gtk.desktop" ];
      welcome-dialog-last-shown-version = "40.1";
    };

    "org/gnome/shell/weather" = {
      automatic-location = true;
    };

    "org/gnome/shell/world-clocks" = {
      locations = "@av []";
    };

    "org/gnome/terminal/legacy/profiles:" = {
      default = "3ccfc57a-67ca-4e54-bf71-030af000fc48";
      list = [ "b1dcc9dd-5262-4d8d-a863-c897e6d979b9" "3ccfc57a-67ca-4e54-bf71-030af000fc48" ];
    };

    "org/gnome/terminal/legacy/profiles:/:3ccfc57a-67ca-4e54-bf71-030af000fc48" = {
      allow-bold = true;
      background-color = "#1E1E21212727";
      bold-color = "#5C5C63637070";
      bold-color-same-as-fg = true;
      cursor-background-color = "#5C5C63637070";
      cursor-colors-set = true;
      cursor-foreground-color = "#1E1E21212727";
      default-size-columns = 96;
      default-size-rows = 28;
      font = "JetBrainsMonoMedium Nerd Font Medium 12";
      foreground-color = "#5C5C63637070";
      palette = [ "#000000000000" "#E0E06C6C7575" "#9898C3C37979" "#D1D19A9A6666" "#6161AFAFEFEF" "#C6C67878DDDD" "#5656B6B6C2C2" "#ABABB2B2BFBF" "#5C5C63637070" "#E0E06C6C7575" "#9898C3C37979" "#D1D19A9A6666" "#6161AFAFEFEF" "#C6C67878DDDD" "#5656B6B6C2C2" "#FFFFFEFEFEFE" ];
      scrollbar-policy = "never";
      use-system-font = true;
      use-theme-background = false;
      use-theme-colors = false;
      use-theme-transparency = false;
      visible-name = "One Dark";
    };

    "org/gnome/tweaks" = {
      show-extensions-notice = false;
    };

    "org/gtk/settings/file-chooser" = {
      date-format = "regular";
      location-mode = "path-bar";
      show-hidden = false;
      show-size-column = true;
      show-type-column = true;
      sidebar-width = 170;
      sort-column = "name";
      sort-directories-first = true;
      sort-order = "ascending";
      type-format = "category";
    };

    "system/locale" = {
      region = "sv_SE.UTF-8";
    };

  };
}
