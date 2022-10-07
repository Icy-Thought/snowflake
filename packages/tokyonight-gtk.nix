{ lib
, stdenv
, fetchFromGitHub
, gtk-engine-murrine
, jdupes
,
}:

stdenv.mkDerivation {
  pname = "tokyonight-gtk-theme";
  version = "unstable-2022-08-31";

  src = fetchFromGitHub {
    owner = "Fausto-Korpsvart";
    repo = "Tokyo-Night-GTK-Theme";
    rev = "6fdc64763ccb1eb1f6e10bf45e4cb70109b577ac";
    hash = "sha256-vU8zNi46DRqLr14KtPyN76vv9zLMrxItKi/ADkgdKto=";
  };

  nativeBuildInputs = [ jdupes ];

  propagatedUserEnvPkgs = [ gtk-engine-murrine ];

  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/themes
    cp -a themes/Tokyonight* $out/share/themes

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
