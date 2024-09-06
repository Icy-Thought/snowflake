{ lib, python3Packages, fetchFromGitHub, }:

python3Packages.buildPythonApplication rec {
  pname = "my_cookies";
  version = "0.1.3";

  src = fetchFromGitHub {
    owner = "kaiwk";
    repo = "${pname}";
    rev = "f9af1e3f90bda0997667489b61da066a134c578e";
    hash = "sha256-9UXOrf9N1UZKY4LiclhTe5N6tHzM10j/n9MiaR1aGZE=";
  };

  propagatedBuildInputs = with python3Packages; [ browser-cookie3 ];

  meta = with lib; {
    description = "Retrieve LeetCode cookies from Chrome with local keyring.";
    homepage = "https://github.com/kaiwk/my_cookies";
    license = licenses.mit;
    platforms = platforms.all;
    # maintainers = [ icy-thought ];
  };
}
