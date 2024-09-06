{ lib, stdenv, fetchFromGitHub, gtk-engine-murrine, jdupes, themeVariant ? [ ]
, iconVariant ? [ ], }:

let
  inherit (builtins) toString;
  inherit (lib.trivial) checkListOfEnum;
in checkListOfEnum "$Rose-Pine: GTK Theme Variants" [
  "Main-B-LB"
  "Main-B"
  "Main-BL-LB"
  "Main-BL"
] themeVariant checkListOfEnum "$RosePine: GTK Theme Variants" [ "" "Moon" ]
iconVariant stdenv.mkDerivation {
  pname = "rose-pine-gtk";
  version = "unstable-2023-02-20";

  src = fetchFromGitHub {
    owner = "Fausto-Korpsvart";
    repo = "Rose-Pine-GKT-Theme";
    rev = "95aa1f2b2cc30495b1fc5b614dc555b3eef0e27d";
    hash = "";
  };

  nativeBuildInputs = [ jdupes ];

  propagatedUserEnvPkgs = [ gtk-engine-murrine ];

  installPhase = let
    gtkTheme = "RosePine-${toString themeVariant}";
    iconTheme = "Rose-Pine-${toString iconVariant}";
  in ''
    runHook preInstall

    mkdir -p $out/share/{icons,themes}

    cp -r $src/themes/${gtkTheme} $out/share/themes
    cp -r $src/icons/${iconTheme} $out/share/icons

    # Duplicate files -> hard-links = reduced install-size!
    jdupes -L -r $out/share

    runHook postInstall
  '';

  meta = with lib; {
    description = "A GTK theme with the Rosé Pine colour palette.";
    homepage = "https://github.com/Fausto-Korpsvart/Rose-Pine-GTK-Theme";
    license = licenses.gpl3Only;
    # maintainers = [ icy-thought ];
    platforms = platforms.all;
  };
}
