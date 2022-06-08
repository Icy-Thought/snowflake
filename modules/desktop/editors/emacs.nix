{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.editors.emacs;
  configDir = config.snowflake.configDir;
in {
  options.modules.desktop.editors.emacs = {
    enable = mkBoolOpt false;
    doom = rec {
      enable = mkBoolOpt false;
      forgeUrl = mkOpt types.str "https://github.com";
      repoUrl = mkOpt types.str "${forgeUrl}/doomemacs/doomemacs";
      configRepoUrl = mkOpt types.str "${forgeUrl}/icy-thought/emacs.d";
    };
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays = with inputs; [emacs.overlay];

    home.services.emacs = {
      enable = true;
      client.enable = true;
    };

    home.programs.emacs = {
      enable = true;
      package = pkgs.emacsNativeComp;
      extraPackages = epkgs: with epkgs; [vterm];
    };

    user.packages = with pkgs; [
      binutils
      gnutls
      zstd
      (mkIf (config.programs.gnupg.agent.enable) pinentry_emacs)
    ];

    # Fonts -> icons + ligatures when specified:
    fonts.fonts = [pkgs.emacs-all-the-icons-fonts];

    # Enable access to doom (tool).
    env.PATH = ["$XDG_CONFIG_HOME/emacs/bin"];

    environment.variables = {
      EMACSDIR = "$XDG_CONFIG_HOME/emacs";
      DOOMDIR = "${configDir}/emacs.d/doom-emacs";
    };

    system.userActivationScripts = mkIf cfg.doom.enable {
      installDoomEmacs = ''
        if [ ! -d "$XDG_CONFIG_HOME/emacs" ]; then
           git clone --depth=1 --single-branch "${cfg.doom.repoUrl}" "$XDG_CONFIG_HOME/emacs"
           git clone "${cfg.doom.configRepoUrl}" "$XDG_CONFIG_HOME/doom"
        fi
      '';
    };

    # Easier frame creation (fish)
    home.programs.fish.functions = {
      eg = "emacs --create-frame $argv & disown";
      ecg = "emacsclient --create-frame $argv & disown";
    };
  };
}
