{
  config,
  options,
  lib,
  pkgs,
  ...
}: let
  inherit (builtins) concatStringsSep readFile;
  inherit (lib.attrsets) attrValues mergeAttrsList;
  inherit (lib.modules) mkIf;

  plugins = [
    "nx-dark-reader"
    "nx-search-engines"
  ];
in {
  options.modules.desktop.browsers.nyxt = let
    inherit (lib.options) mkEnableOption;
  in {enable = mkEnableOption "The hacker's browser";};

  config = mkIf config.modules.desktop.browsers.nyxt.enable {
    user.packages = attrValues {
      inherit (pkgs) nyxt;
    };

    home.dataFile = mergeAttrsList (map
      (extension: {
        "nyxt/extensions/${extension}".source = pkgs.sources.${extension};
      })
      plugins);

    home.configFile = let
      nyxtConfDir = "${config.snowflake.configDir}/nyxt";
    in {
      nyxt-conf = {
        text =
          ''
            (in-package #:nyxt-user)

            (defmacro load-extensions (&rest extensions)
              "Helper macro to load extensions along with config files with the same name."
              `(progn ,@(loop for extension
                              in extensions
                              collect `(nyxt:define-nyxt-user-system-and-load
                                           ,(alexandria:symbolicate 'nyxt-user/ extension '-proxy)
                                           :description ,(format t "This proxy system saves us if ~a fails to load.
                                        Otherwise it will break all the config loading." extension)
                                           :depends-on (,extension)))))

            (load-extensions ${concatStringsSep "\n" (map (p: ":" + p) plugins)})
          ''
          + readFile "${nyxtConfDir}/config.lisp";
        target = "nyxt/config.lisp";
      };
      nyxt-aesthetics = let
        inherit (config.modules.themes) active font;
        inherit (config.modules.themes.colors.main) normal types;
      in {
        text =
          readFile "${nyxtConfDir}/aesthetics.lisp"
          + (
            if (active != null)
            then ''
              (define-configuration browser
                  ((theme (make-instance
                           'theme:theme
                           :background-color "${types.bg}"
                           :background-color+ "#${types.bg}"
                           :background-color- "#${types.bg}"

                           :primary-color "${types.fg}"
                           :primary-color+ "${types.fg}"
                           :primary-color- "${types.fg}"

                           :secondary-color "${normal.black}"
                           :secondary-color+ "${normal.black}"
                           :secondary-color- "${normal.black}"

                           :action-color "${normal.yellow}"
                           :action-color+ "${normal.yellow}"
                           :action-color- "${normal.yellow}"

                           :success-color "${normal.green}"
                           :success-color+ "${normal.green}"
                           :success-color- "${normal.green}"

                           :warning-color "${normal.red}"
                           :warning-color+ "${normal.red}"
                           :warning-color- "${normal.red}"

                           :highlight-color "${types.highlight}"
                           :highlight-color+ "${types.highlight}"
                           :highlight-color- "${types.highlight}"

                           :codeblock-color "${types.bg}"
                           :codeblock-color+ "${types.bg}"
                           :codeblock-color- "${types.bg}"

                           :contrast-text-color "${types.bg}"
                           :contrast-text-color+ "${types.bg}"
                           :contrast-text-color- "${types.bg}"

                           :text-color "${types.fg}"
                           :text-color+ "${types.fg}"
                           :text-color- "${types.fg}"

                           :font-family "${font.sans.family}"))))

              (define-configuration nx-dark-reader:dark-reader-mode
                  ((nxdr:background-color "${types.bg}")
                   (nxdr:text-color "${types.fg}")
                   (nxdr:selection-color "${types.highlight}")))
            ''
            else ""
          );
        target = "nyxt/aesthetics.lisp";
      };
      nyxt-bindings = {
        source = "${nyxtConfDir}/bindings.lisp";
        target = "nyxt/bindings.lisp";
      };
    };
  };
}
