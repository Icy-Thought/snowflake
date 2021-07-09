{ mkDerivation, base, containers, data-default, directory, fetchgit, filepath
, lib, mtl, process, QuickCheck, quickcheck-classes, setlocale, transformers
, unix, utf8-string, X11, typed-process }:

mkDerivation {
  pname = "xmonad";
  version = "0.16.9999";
  src = fetchgit {
    url = "https://github.com/xmonad/xmonad.git";
    rev = "af354f7528ada1de451365a0f5138ef10a318360";
    sha256 = "sq+hDiMfzfYRvzYucpmNt4u60QT9HXH+rJ89jptyMSI=";
    fetchSubmodules = true;
  };

  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base
    containers
    data-default
    directory
    filepath
    mtl
    process
    setlocale
    transformers
    unix
    utf8-string
    X11
    typed-process
  ];

  executableHaskellDepends = [ base mtl unix X11 ];
  testHaskellDepends = [ base containers QuickCheck quickcheck-classes X11 ];

  postInstall = ''
    install -D man/xmonad.1 ''${!outputDoc}/share/man/man1/xmonad.1
    install -D man/xmonad.hs ''${!outputDoc}/share/doc/$name/sample-xmonad.hs
  '';

  homepage = "http://xmonad.org";
  description = "A tiling window manager";
  license = lib.licenses.bsd3;
}
