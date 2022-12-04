{ lib
, stdenv
, fetchFromGitHub
, gtk-engine-murrine
, jdupes
, themeVariants ? [ ]
}:

lib.checkListOfEnum "$Tokyonight: GTK Theme Variants" [
  "Dark-B-LB"
  "Dark-B"
  "Dark-BL-LB"
  "Dark-BL"
  "Light-B-LB"
  "Light-B"
  "Light-BL-LB"
  "Light-BL"
  "Moon-B-LB"
  "Moon-B"
  "Moon-BL-LB"
  "Moon-BL"
  "Storm-B-LB"
  "Storm-B"
  "Storm-BL-LB"
  "Storm-BL"
]
  themeVariants

  stdenv.mkDerivation
{
  pname = "tokyonight-gtk-theme";
  version = "unstable-2022-10-21";

  src = fetchFromGitHub {
    owner = "Fausto-Korpsvart";
    repo = "Tokyo-Night-GTK-Theme";
    rev = "6247aafad59a9231d8638de2a09174779761ffeb";
    hash = "sha256-vxSu5FW4XSvOyHK/Zl4Wh2Tik6cKBSE22C3AE9cRkoE=";
  };

  nativeBuildInputs = [ jdupes ];

  propagatedUserEnvPkgs = [ gtk-engine-murrine ];

  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/{gtk-4.0,themes}

    # GTK-4.0
    cp -r $src/themes/Gnome42/Tokyonight-${builtins.toString themeVariants}.css $out/share/gtk-4.0

    # GTK-3.0
    cp -r $src/themes/Tokyonight-${builtins.toString themeVariants} $out/share/themes

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
