{
  options,
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib.attrsets) attrValues optionalAttrs;
  inherit (lib.modules) mkIf mkMerge;

  cfg = config.modules.desktop.toolset.fileBrowse;
in {
  options.modules.desktop.toolset.fileBrowse = let
    inherit (lib.options) mkEnableOption;
  in {
    dolphin.enable = mkEnableOption "KDE Plasma file-manager";
    nautilus.enable = mkEnableOption "Gnome file-manager";
    thunar.enable = mkEnableOption "GTK+ file-manager";
  };

  config = mkMerge [
    {
      services.gvfs.enable = true;

      environment.systemPackages = attrValues ({}
        // optionalAttrs (cfg.dolphin.enable) {
          inherit (pkgs) dolphin dolphin-plugins;
        }
        // optionalAttrs (cfg.nautilus.enable) {
          inherit (pkgs.gnome) nautilus;
        }
        // optionalAttrs (cfg.thunar.enable) {
          inherit
            (pkgs.xfce)
            thunar
            thunar-volman
            thunar-archive-plugin
            thunar-media-tags-plugin
            ;
        });
    }

    (mkIf cfg.thunar.enable {
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
