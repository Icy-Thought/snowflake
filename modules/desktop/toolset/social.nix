{ config
, options
, lib
, pkgs
, ...
}:
with lib;
with lib.my;

let cfg = config.modules.desktop.toolset.social;
in {
  options.modules.desktop.toolset.social = {
    common.enable = mkBoolOpt true;
  };

  config = mkMerge [
    (mkIf cfg.common.enable {
      user.packages = with pkgs;
        let
          # Copyright (c) 2022 roosemberth. All Rights Reserved.
          element-desktop' = pkgs.symlinkJoin {
            name = "element-desktop-in-dataHome";
            paths = [ element-desktop ];
            nativeBuildInputs = [ pkgs.makeWrapper ];
            postBuild = ''
              wrapProgram "$out/bin/element-desktop" \
                --add-flags '--profile-dir $XDG_DATA_HOME/Element'
            '';
          };
        in
        [
          element-desktop'
          unstable.discord
          signal-desktop
          tdesktop
        ];

      home.configFile = {
        # Plugins:
        bd-tracker-prevention = {
          target = "BetterDiscord/plugins/tracker-prevention.plugin.js";
          source =
            let rev = "7d9067e39ce576edea36d96aab71e9cdfe7ff9f1";
            in builtins.fetchurl {
              url = "https://raw.githubusercontent.com/rauenzi/BetterDiscordAddons/${rev}/Plugins/DoNotTrack/DoNotTrack.plugin.js";
              sha256 = "0klg5m0b641vrlkajqvw224b7acg2jxgfqaflxf4b14xa8r8ajh2";
            };
        };
        bd-translator = {
          target = "BetterDiscord/plugins/translator.plugin.js";
          source =
            let rev = "43a27d43f46c7279e6ed812992df3440b8873a69";
            in builtins.fetchurl {
              url = "https://raw.githubusercontent.com/mwittrien/BetterDiscordAddons/${rev}/Plugins/Translator/Translator.plugin.js";
              sha256 = "1y57h4l4szslccdyznc4y433cyl1dvq1gvrwzxbwlhawz2hg5as2";
            };
        };
        bd-image-utils = {
          target = "BetterDiscord/plugins/image-utilities.plugin.js";
          source =
            let rev = "06a85d29e18efa7209860588935ccca5080c0800";
            in builtins.fetchurl {
              url = "https://raw.githubusercontent.com/mwittrien/BetterDiscordAddons/${rev}/Plugins/ImageUtilities/ImageUtilities.plugin.js";
              sha256 = "0ji1w5f7mi1a5s81kj1nw4psls742zpi4sffdnvwldxx0i6lmm78";
            };
        };
        bd-quick-edit = {
          target = "BetterDiscord/plugins/quick-edit.plugin.js";
          source =
            let rev = "42b3d9adb0c0f9613b2ccf6969481c677750e6ae";
            in builtins.fetchurl {
              url = "https://raw.githubusercontent.com/Farcrada/DiscordPlugins/${rev}/Double-click-to-edit/DoubleClickToEdit.plugin.js";
              sha256 = "03v59nlypcg64knd099xmyklcivb945bpmff8vd6yn6ilgick6nj";
            };
        };
        bd-quick-mention = {
          target = "BetterDiscord/plugins/quick-mention.plugin.js";
          source =
            let rev = "7e7981b982fac6f18df9b052d0e74e7e01450aca";
            in builtins.fetchurl {
              url = "https://raw.githubusercontent.com/mwittrien/BetterDiscordAddons/${rev}/Plugins/QuickMention/QuickMention.plugin.js";
              sha256 = "0nww5gb4ki2yz1y7q03p5zvndnnlzqgc9hj075wab5cnh2zicsmh";
            };
        };
        bd-hide-disabled-emojies = {
          target = "BetterDiscord/plugins/hide-disabled-emojies.plugin.js";
          source =
            let rev = "45cc2cba6bd0fc4230b918d0599c969a6d141a36";
            in builtins.fetchurl {
              url = "https://raw.githubusercontent.com/rauenzi/BetterDiscordAddons/${rev}/Plugins/HideDisabledEmojis/HideDisabledEmojis.plugin.js";
              sha256 = "0m2sr8wxyzi1zpr19g0s7hr5yixabzjg3c1sv7jfbybqxjy5kvjm";
            };
        };
        bd-display-connections = {
          target = "BetterDiscord/plugins/display-connetions.plugin.js";
          source =
            let rev = "0005ab91539556d464209ec7fc02a97331f35115";
            in builtins.fetchurl {
              url = "https://raw.githubusercontent.com/mwittrien/BetterDiscordAddons/${rev}/Plugins/ShowConnections/ShowConnections.plugin.js";
              sha256 = "0jj4ncarkw3n8s7fds6iapmxmdx125x4ahyaxkq0chz5n3ylfv74";
            };
        };

        # Themes:
        bd-theme = {
          target = "BetterDiscord/themes/catppuccin-moccha.theme.css";
          source = builtins.fetchurl {
            url = "https://raw.githubusercontent.com/catppuccin/discord/main/themes/mocha.theme.css";
            sha256 = "1w921c6zg5xvkf52x642psnqpaannbd28cc37dfzasbplw7ghl2x";
          };
        };
      };
    })
  ];
}
