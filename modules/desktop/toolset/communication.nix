{ options, config, lib, pkgs, ... }:

let
  inherit (lib.attrsets) attrValues;
  inherit (lib.lists) optionals;
  inherit (lib.modules) mkIf mkMerge;
  inherit (lib.strings) concatStringsSep;

  cfg = config.modules.desktop.toolset.communication;
  desktop = config.modules.desktop;
  mailDir = "${config.hm.xdg.dataHome}/mail";
in {
  options.modules.desktop.toolset.communication = let
    inherit (lib.options) mkEnableOption mkOption;
    inherit (lib.types) nullOr enum;
  in {
    base.enable = mkEnableOption "cross-platform clients";
    mu4e.enable = mkEnableOption "a full-featured e-mail client";
    discord.enable = mkEnableOption "discord client" // {
      default = cfg.base.enable;
    };
    matrix = {
      withDaemon = {
        enable = mkEnableOption "matrix daemon for ement.el" // {
          default = config.modules.desktop.editors.emacs.enable
            && !cfg.matrix.withClient.enable;
        };
      };
      withClient = {
        enable = mkEnableOption "rust-based matrix client" // {
          default = cfg.base.enable && !cfg.matrix.withDaemon.enable;
        };
        package = mkOption {
          type = nullOr (enum [ "element" "fractal" ]);
          default = "element";
          description = "What display protocol to use.";
        };
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.base.enable {
      user.packages = attrValues { inherit (pkgs) signal-desktop; };
    })

    (mkIf cfg.mu4e.enable {
      hm.accounts.email = {
        maildirBasePath = mailDir;
        accounts.${config.user.name} = let mailAddr = "IcyThought@disroot.org";
        in {
          realName = "${config.user.name}";
          userName = "${mailAddr}";
          address = "${mailAddr}";
          passwordCommand = "cat /run/agenix/mailingQST";
          primary = true;

          flavor = "plain";
          imap = {
            host = "disroot.org";
            port = 993;
          };
          smtp = {
            host = "disroot.org";
            port = 465;
          };
          gpg = {
            key = "2E690B8644FE29D8237F6C42B593E438DDAB3C66";
            encryptByDefault = false;
            signByDefault = true;
          };
          mbsync = {
            enable = true;
            create = "both";
            expunge = "both";
            patterns = [ "*" ];
          };
          msmtp = {
            enable = true;
            extraConfig.auth = "login";
          };
          mu.enable = true;
        };
      };

      hm.programs = {
        mbsync.enable = true;
        msmtp.enable = true;
        mu.enable = true;
      };
    })

    (mkIf cfg.matrix.withDaemon.enable {
      hm.nixpkgs = {
        # :WARN| TEMPORARY/INSECURE SOLUTION...
        config.permittedInsecurePackages = [ "olm-3.2.16" ];
      };

      hm.services.pantalaimon = {
        enable = true;
        settings = {
          Default = {
            LogLevel = "Debug";
            SSL = true;
          };
          local-matrix = {
            Homeserver = "https://matrix.org";
            ListenAddress = "localhost";
            ListenPort = 8009;
            IgnoreVerification = true;
            UseKeyring = false;
          };
        };
      };
    })

    (mkIf cfg.matrix.withClient.enable {
      user.packages = let
        inherit (pkgs) makeWrapper symlinkJoin element-desktop;
        element-desktop' = symlinkJoin {
          name = "element-desktop-in-dataHome";
          paths = [ element-desktop ];
          nativeBuildInputs = [ makeWrapper ];
          postBuild = ''
            wrapProgram "$out/bin/element-desktop" \
              --add-flags '--profile-dir $XDG_DATA_HOME/Element'
          '';
        };
      in if (cfg.matrix.withClient.package == "element") then
        [ element-desktop' ]
      else
        [ pkgs.fractal-next ];
    })

    (mkIf cfg.discord.enable { user.packages = [ pkgs.vesktop ]; })
  ];
}
