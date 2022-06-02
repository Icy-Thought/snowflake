{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.media.browser;
  configDir = config.snowflake.configDir;
in {
  options.modules.desktop.media.browser = {
    dolphin.enable = mkBoolOpt false;
    nautilus.enable = mkBoolOpt false;
    thunar.enable = mkBoolOpt false;
  };

  config = mkMerge [
    {services.gvfs.enable = true;}

    (mkIf cfg.dolphin.enable {
      environment.systemPackages = with pkgs.libsForQt5; [
        dolphin
        dolphin-plugins
      ];
    })

    (mkIf cfg.nautilus.enable {
      environment.systemPackages = with pkgs.gnome; [nautilus];
    })

    (mkIf cfg.thunar.enable {
      environment.systemPackages = with pkgs.xfce; [
        thunar
        thunar-volman
        thunar-archive-plugin
        thunar-media-tags-plugin
      ];

      services.tumbler.enable = true;

      home.configFile = {
        "Thunar/uca.xml".text = ''
          <?xml version="1.0" encoding="UTF-8"?>
          <actions>
          <action>
                  <icon>kitty</icon>
                  <name>Launch Kitty Here</name>
                  <unique-id>1653079815094995-1</unique-id>
                  <command>kitty --working-directory %f</command>
                  <description>Example for a custom action</description>
                  <patterns>*</patterns>
                  <startup-notify/>
                  <directories/>
          </action>
          </actions>
        '';

        "xfce4/xfconf/xfce-perchannel-xml/thunar.xml".text = ''
          <?xml version="1.0" encoding="UTF-8"?>

          <channel name="thunar" version="1.0">
            <property name="last-view" type="string" value="ThunarIconView"/>
            <property name="last-icon-view-zoom-level" type="string" value="THUNAR_ZOOM_LEVEL_150_PERCENT"/>
            <property name="last-window-width" type="int" value="946"/>
            <property name="last-window-height" type="int" value="503"/>
            <property name="last-window-maximized" type="bool" value="false"/>
            <property name="last-side-pane" type="string" value="ThunarTreePane"/>
            <property name="last-separator-position" type="int" value="213"/>
            <property name="last-location-bar" type="string" value="ThunarLocationEntry"/>
            <property name="misc-single-click" type="bool" value="false"/>
            <property name="misc-thumbnail-mode" type="string" value="THUNAR_THUMBNAIL_MODE_ALWAYS"/>
            <property name="misc-text-beside-icons" type="bool" value="false"/>
            <property name="misc-date-style" type="string" value="THUNAR_DATE_STYLE_SHORT"/>
            <property name="shortcuts-icon-size" type="string" value="THUNAR_ICON_SIZE_32"/>
            <property name="tree-icon-size" type="string" value="THUNAR_ICON_SIZE_24"/>
            <property name="last-menubar-visible" type="bool" value="false"/>
          </channel>
        '';
      };
    })
  ];
}
