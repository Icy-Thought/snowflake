{ lib
, stdenv
, fetchFromGitHub
, gtk-engine-murrine
, jdupes
,
}:
stdenv.mkDerivation rec {
  pname = "tokyonight-gtk-theme";
  version = "unstable-2022-08-12";

  src = fetchFromGitHub {
    owner = "Fausto-Korpsvart";
    repo = "Tokyo-Night-GTK-Theme";
    rev = "6bbb8954b62cd6fb696f7811e7e8049f89c51393";
    sha256 = "5dkkfCS9d9detnTn86WcCUQCgDFJ9NI/tyHg8jqxKOI=";
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
    platforms = platforms.all;
    maintainers = [ ];
  };
}
