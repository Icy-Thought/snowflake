{ lib, stdenvNoCC, fetchFromGitHub, texlive }:

stdenvNoCC.mkDerivation rec {
  pname = "BMC";
  version = "2021-04-17";
  passthru.tlType = "run";

  src = fetchFromGitHub {
    owner = "tecosaur";
    repo = pname;
    rev = "b03bc88416343295c1d17c7aaa7f4059a08ac459";
    sha256 = "YwhUVLF90K4ZGK1nvtVuZPzFikhSit7sqy43wHxe+jo=";
  };

  dontBuild = true;
  dontConfigure = true;

  installPhase = ''
    mkdir -p $out/tex/latex
    cp -va *.sty *.cls $out/tex/latex
  '';

  meta = with lib; {
    description =
      "My bespoke, multipurpose class; designed for general use in LaTeX documents";
    homepage = "https://github.com/tecosaur/BMC";
    license = licenses.lppl13c;
    maintainers = with maintainers; [ Icy-Thought ];
    platforms = platforms.linux;
  };
}
