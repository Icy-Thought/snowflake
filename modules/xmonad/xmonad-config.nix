{ mkDerivation, base, containers, hpack, lib, unix, utf8-string, X11, xmobar
, xmonad, xmonad-contrib }:

mkDerivation {
  pname = "xmonad-config";
  version = "0.16";

  src = ./src;
  isLibrary = false;
  isExecutable = true;
  libraryToolDepends = [ hpack ];
  executableHaskellDepends =
    [ base containers unix utf8-string X11 xmobar xmonad xmonad-contrib ];

  prePatch = "hpack";
  homepage = "https://github.com/Icy-Thought/Snowflake/modules/xmonad";
  description = "Xmonad configuration based on xmonad-atif";
  license = lib.licenses.gpl3Only;

}
