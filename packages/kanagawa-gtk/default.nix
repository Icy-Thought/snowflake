{ lib, stdenv, fetchFromGitHub, gtk-engine-murrine, themeVariants ? [ "BL" ], }:

lib.checkListOfEnum "$Kanagawa: GTK Theme Variants" [ "B" "B-LB" "BL" "BL-LB" ]
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

  propagatedUserEnvPkgs = [ gtk-engine-murrine ];

  installPhase = let gtkTheme = "Kanagawa-${builtins.toString themeVariants}";
  in ''
    runHook preInstall

    mkdir -p $out/share/{icons,themes}

    cp -r $src/themes/${gtkTheme} $out/share/themes
    cp -r $src/icons/Kanagawa $out/share/icons

    runHook postInstall
  '';

  meta = with lib; {
    description = "A GTK theme based on the Kanagawa colour palette";
    homepage = "https://github.com/Fausto-Korpsvart/Kanagawa-GTK-Theme";
    license = licenses.gpl3Only;
    # maintainers = [ icy-thought ];
    platforms = platforms.all;
  };
}
