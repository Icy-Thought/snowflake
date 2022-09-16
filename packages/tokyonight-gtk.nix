{
  lib,
  stdenv,
  fetchFromGitHub,
  gtk-engine-murrine,
  jdupes,
}:
stdenv.mkDerivation {
  pname = "tokyonight-gtk-theme";
  version = "unstable-2022-08-31";

  src = fetchFromGitHub {
    owner = "Fausto-Korpsvart";
    repo = "Tokyo-Night-GTK-Theme";
    rev = "12bd8dbc8316582325e382f6143532f6e6a72a09";
    sha256 = "1mVXtWA5MB6Vj0/EGhvEYpj5avHsQvzHhWFeu1WE+VU=";
  };

  nativeBuildInputs = [jdupes];

  propagatedUserEnvPkgs = [gtk-engine-murrine];

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
    maintainers = [];
  };
}
