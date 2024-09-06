{ lib, stdenv, fetchFromGitHub, }:
stdenv.mkDerivation {
  pname = "fcitx5-catppuccin";
  version = "0.0.1";

  src = fetchFromGitHub {
    owner = "catppuccin";
    repo = "fcitx5";
    rev = "ce244cfdf43a648d984719fdfd1d60aab09f5c97";
    hash = "sha256-uFaCbyrEjv4oiKUzLVFzw+UY54/h7wh2cntqeyYwGps=";
  };

  dontBuild = true;

  installPhase = ''
    mkdir -p $out/share/fcitx5/themes
    cp -r ./src/* $out/share/fcitx5/themes
  '';

  meta = with lib; {
    description = "Soothing pastel theme for Fcitx5";
    homepage = "https://github.com/catppuccin/fcitx5";
    license = with licenses; [ mit ];
    maintainers = [ ];
  };
}
