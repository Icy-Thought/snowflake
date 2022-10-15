{ lib
, stdenv
, fetchFromGitHub
, gtk-engine-murrine
, jdupes
,
}:

stdenv.mkDerivation {
  pname = "tokyonight-gtk-theme";
  version = "unstable-2022-10-13";

  src = fetchFromGitHub {
    owner = "Fausto-Korpsvart";
    repo = "Tokyo-Night-GTK-Theme";
    rev = "517716f1da4520e9db37c7d86907c54f76dfb936";
    hash = "sha256-fGjFKEOL9F1bFWRBKC1tJfDWIucRoU04YNHGuVlcrLQ=";
  };

  nativeBuildInputs = [ jdupes ];

  propagatedUserEnvPkgs = [ gtk-engine-murrine ];

  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/themes

    # GTK-3.0 && GTK-2.0
    cp -a themes/Tokyonight-* $out/share/themes

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
