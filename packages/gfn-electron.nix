{ lib
, makeDesktopItem
, copyDesktopItems
, stdenvNoCC
, fetchurl
, appimageTools
,
}:
let
  pname = "gfn-electron";
  version = "1.10.0";
  src = fetchurl {
    url = "https://github.com/hmlendea/gfn-electron/releases/download/v${version}/geforcenow-electron_${version}_linux.AppImage";
    hash = "sha256-t+IqUZrgXlyrmBJ5HYNYyn0Th5rezbg3cepPwg5S8tg=";
  };
  appimage = appimageTools.wrapType2 { inherit version pname src; };
  appimage-contents = appimageTools.extractType2 { inherit version pname src; };
in
stdenvNoCC.mkDerivation {
  inherit version pname;
  src = appimage;

  nativeBuildInputs = [ copyDesktopItems ];

  desktopItems = [
    (makeDesktopItem {
      name = "gfn-electron";
      desktopName = "GeForce NOW";
      comment = "Stream games using the Nvidia GeForce NOW service";
      exec = "${appimage}/bin/${pname}-${version}";
      icon = "${appimage-contents}/geforcenow-electron.png";
      terminal = false;
      type = "Application";
      categories = [ "Network" "Game" ];
    })
  ];

  installPhase = ''
    runHook preInstall

    mkdir -p $out/
    cp -r bin $out/bin

    runHook postInstall
  '';

  meta = with lib; {
    description = "Linux Desktop client for Nvidia's GeForce NOW game streaming service";
    homepage = "https://github.com/hmlendea/gfn-electron";
    license = licenses.gpl3Only;
    platforms = platforms.linux;
    maintainers = with maintainers; [ Icy-Thought ];
  };
}
