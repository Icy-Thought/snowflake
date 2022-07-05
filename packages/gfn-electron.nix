{
  lib,
  makeDesktopItem,
  copyDesktopItems,
  stdenvNoCC,
  fetchurl,
  appimageTools,
}: let
  pname = "gfn-electron";
  version = "1.8.0";
  src = fetchurl {
    url = "https://github.com/hmlendea/gfn-electron/releases/download/v${version}/geforcenow-electron_${version}_linux.AppImage";
    sha256 = "vMo4FU8Q61MDpJIv9Qwu7xB8ZapXXhXFj1atn2C8A84=";
  };
  appimage = appimageTools.wrapType2 {inherit version pname src;};
  appimage-contents = appimageTools.extractType2 {inherit version pname src;};
in
  stdenvNoCC.mkDerivation {
    inherit version pname;
    src = appimage;

    nativeBuildInputs = [copyDesktopItems];

    desktopItems = [
      (makeDesktopItem {
        name = "gfn-electron";
        desktopName = "GeForce NOW";
        comment = "Stream games using the Nvidia GeForce NOW service";
        exec = "${appimage}/bin/${pname}-${version}";
        icon = "${appimage-contents}/geforcenow-electron.png";
        terminal = false;
        type = "Application";
        categories = ["Network" "Game"];
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
      maintainers = with maintainers; [Icy-Thought];
    };
  }
