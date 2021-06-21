{ config, lib, ... }:
{
  services = {
    udiskie = {
      enable = true;
      tray = "always";
    };

    picom = {
      enable = true;
      backend = "glx";
      # experimentalBackends = true;
      # package = pkgs.nur.repos.reedrw.picom-next-ibhagwan;
    };

    dunst = {
      enable = true;
      settings = {
        urgency_low.timeout = 10;
        urgency_normal.timeout = 10;
        urgency_critical.timeout = 10;
        
        global = {
          transparency = 10;
          alignment = "left";
          geometry = "300x5-30+20";
          font = "Iosevka SemiBold";
          frame_color = "#959DCB";
          separator_color = "#959DCB";
        };

        urgency_low = {
          background = "#444267";
          foreground = "#676E95";
        };

        urgency_normal = {
          background = "#32374D";
          foreground = "#959DCB";
        };

        urgency_critical = {
          background = "#F07178";
          foreground = "#959DCB";
        };
      };
    };

  };
}
