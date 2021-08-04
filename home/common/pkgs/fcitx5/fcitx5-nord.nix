{ stdenv, fetchgit }:
stdenv.mkDerivation rec {
  name = "fcitx5-nord";
  version = "20210727";

  src = fetchgit {
    url = "https://github.com/tonyfettes/fcitx5-nord";
    rev = "bdaa8fb723b8d0b22f237c9a60195c5f9c9d74d1";
    sha256 = "qVo/0ivZ5gfUP17G29CAW0MrRFUO0KN1ADl1I/rvchE=";
  };

  unpackPhase = "mkdir $out";
  installPhase = "cd ${src} && cp -r * $out";
}
