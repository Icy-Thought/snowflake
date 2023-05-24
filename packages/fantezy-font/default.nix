{
  lib,
  stdenv,
}:
stdenv.mkDerivation {
  pname = "fantezy-font";
  version = "0.0.1";
  src = ./.;
  dontConfigure = true;

  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/fonts/truetype
    cp $src/*.ttf $out/share/fonts/truetype

    runHook postInstall
  '';

  meta = with lib; {
    description = "An Arabic font for cursive style admirers!";
    homepage = "";
    license = licenses.ofl;
    # maintainers = with maintainers; [ Icy-Thought ];
    platforms = platforms.all;
  };
}
