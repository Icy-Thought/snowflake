{
  lib,
  stdenv,
  fetchFromGitHub,
  gtk-engine-murrine,
  jdupes,
  variant ? "Dark-BL",
}: let
  inherit (builtins) toString;
  inherit (lib.trivial) checkListOfEnum;
  validVariants = ["Dark-B-LB" "Dark-B" "Dark-BL-LB" "Dark-BL" "Storm-B-LB" "Storm-B" "Storm-BL-LB" "Storm-BL"];

  pname = "tokyonight-gtk-theme";
in
  checkListOfEnum "${pname}: GTK Theme Variants" validVariants [variant]
  stdenv.mkDerivation {
    pname = "tokyonight-gtk-theme";
    version = "unstable-2022-12-09";

    src = fetchFromGitHub {
      owner = "Fausto-Korpsvart";
      repo = "Tokyo-Night-GTK-Theme";
      rev = "d17eec24180b890bc4a9aa64162074b1bfc7258a";
      hash = "sha256-b35J6NsFkUNM/yxMe7bi0kpyuI/pGLnCywCEDLHLf5A=";
    };

    nativeBuildInputs = [jdupes];

    propagatedUserEnvPkgs = [gtk-engine-murrine];

    installPhase = let
      gtkTheme = "Tokyonight-${toString variant}";
    in ''
      runHook preInstall

      mkdir -p $out/share/themes

      cp -r $src/themes/${gtkTheme} $out/share/themes

      # Duplicate files -> hard-links = reduced install-size!
      jdupes -L -r $out/share

      runHook postInstall
    '';

    meta = with lib; {
      description = "A GTK theme based on the Tokyo Night colour palette";
      homepage = "https://github.com/Fausto-Korpsvart/Tokyo-Night-GTK-Theme";
      license = licenses.gpl3Only;
      # maintainers = [ Icy-Thought ];
      platforms = platforms.all;
    };
  }
