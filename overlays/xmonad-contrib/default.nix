{ mkDerivation, base, bytestring, containers, directory, extensible-exceptions
, fetchgit, filepath, mtl, old-locale, old-time, process, random, semigroups
, stdenv, unix, utf8-string, X11, X11-xft, xmonad }:

mkDerivation {
  pname = "xmonad-contrib";
  version = "0.17";
  src = fetchgit {
    url = "https://github.com/xmonad/xmonad-contrib";
    rev = "1351f9a931f53e9f1e16c566c70cb8fa98f97785";
    sha256 = "ZX7YU/mp/ORufbL4whnD1vBXVcMqOv8aN+x+lQ7HdOo=";
    fetchSubmodules = true;
  };

  libraryHaskellDepends = [
    base
    bytestring
    containers
    directory
    extensible-exceptions
    filepath
    mtl
    old-locale
    old-time
    process
    random
    semigroups
    unix
    utf8-string
    X11
    X11-xft
    xmonad
  ];

  homepage = "http://xmonad.org/";
  description = "Third party extensions for xmonad";
  license = stdenv.lib.licenses.bsd3;
}
