{ stdenv, fetchgit }:
stdenv.mkDerivation rec {
  name = "fcitx5-nord";
  version = "bab8b8a";

  src = fetchgit {
    url = "https://github.com/tonyfettes/fcitx5-nord";
    rev = "${version}d4bd39a400b997b4b7d3a12f4608c15e7";
    sha256 = "qVo/0ivZ5gfUP17G29CAW0MrRFUO0KN1ADl1I/rvchE=";
  };

  unpackPhase = "mkdir $out";
  installPhase = "cd ${src} && cp -r * $out";
}
