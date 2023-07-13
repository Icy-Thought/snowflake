{
  lib,
  stdenv,
  fetchFromGitHub,
  gtk-engine-murrine,
  jdupes,
  themeVariants ? [],
}: let
  inherit (lib) checkListOfEnum;
  inherit (builtins) toString;
in
  checkListOfEnum "$Kanagawa: GTK Theme Variants" [
    "B"
    "B-LB"
    "BL"
    "BL-LB"
  ]
  themeVariants
  stdenv.mkDerivation {
    pname = "kanagawa-gtk";
    version = "unstable-2023-07-04";

    src = fetchFromGitHub {
      owner = "Fausto-Korpsvart";
      repo = "Kanagawa-GKT-Theme";
      rev = "35936a1e3bbd329339991b29725fc1f67f192c1e";
      hash = "sha256-BZRmjVas8q6zsYbXFk4bCk5Ec/3liy9PQ8fqFGHAXe0";
    };

    nativeBuildInputs = [jdupes];

    propagatedUserEnvPkgs = [gtk-engine-murrine];

    installPhase = let
      gtkTheme = "Kanagawa-${toString themeVariants}";
    in ''
      runHook preInstall

      mkdir -p $out/share/{icons,themes}

      cp -r $src/themes/${gtkTheme} $out/share/themes
      cp -r $src/icons/Kanagawa $out/share/icons

      # Duplicate files -> hard-links = reduced install-size!
      jdupes -L -r $out/share

      runHook postInstall
    '';

    meta = with lib; {
      description = "A GTK theme based on the Kanagawa colour palette";
      homepage = "https://github.com/Fausto-Korpsvart/Kanagawa-GTK-Theme";
      license = licenses.gpl3Only;
      # maintainers = [ Icy-Thought ];
      platforms = platforms.all;
    };
  }
