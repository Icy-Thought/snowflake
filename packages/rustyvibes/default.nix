{ lib, rustPlatform, fetchFromGitHub, pkg-config, alsa-lib, xorg, }:

rustPlatform.buildRustPackage rec {
  pname = "rustyvibes";
  version = "1.0.9";

  src = fetchFromGitHub {
    owner = "KunalBagaria";
    repo = "${pname}";
    rev = "v${version}";
    hash = "sha256-xQxCdAYdvagdUo70s3F+fIfvpqUZrlFt3bfL5cfIocU=";
  };

  cargoLock = { lockFile = ./Cargo.lock; };

  postPatch = ''
    ln -s ${./Cargo.lock} Cargo.lock
  '';

  nativeBuildInputs = [ pkg-config ];

  buildInputs = [ alsa-lib xorg.libX11 xorg.libXi xorg.libXtst ];

  meta = with lib; {
    description =
      "A Rust CLI that makes mechanical keyboard sound effects on every key press";
    homepage = "https://github.com/KunalBagaria/rustyvibes";
    license = licenses.mit;
    # maintainers = with maintainers; [icy-thought];
    mainProgram = "rustyvibes";
  };
}
