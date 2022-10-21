{ lib, stdenv, fetchFromGitHub, gtk-engine-murrine, jdupes, themeVariants ? [ ]
}:

let pname = "tokyonight-gtk-theme";

in lib.checkListOfEnum "${pname}: theme variants" [
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
] themeVariants

stdenv.mkDerivation {
  pname = pname;
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

    # GTK-Theme
    cp -a $src/themes/Tokyonight-${
      builtins.toString themeVariants
    } $out/share/themes/Tokyonight-${builtins.toString themeVariants}

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
