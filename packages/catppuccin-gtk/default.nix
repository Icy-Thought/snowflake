{
  lib,
  stdenv,
  fetchFromGitHub,
  gtk-engine-murrine,
  jdupes,
  sassc,
  accent ? ["pink"],
  shade ? "dark",
  size ? "standard",
}: let
  validAccents = ["purple" "pink" "red" "orange" "yellow" "green" "teal" "grey"];
  validShades = ["light" "dark"];
  validSizes = ["standard" "compact"];

  single = x: lib.optional (x != null) x;
  pname = "Catppuccin";
in
  lib.checkListOfEnum "${pname} Valid theme accent(s)" validAccents accent
  lib.checkListOfEnum "${pname} Valid shades" validShades (single shade)
  lib.checkListOfEnum "${pname} Valid sizes" validSizes (single size)

  stdenv.mkDerivation {
    pname = "${pname}";
    version = "unstable-2024-06-20";

    src = fetchFromGitHub {
      owner = "Fausto-Korpsvart";
      repo = "Catppuccin-GTK-Theme";
      rev = "320ff909ac7dacaf14193b6b3dcfa3254ab19f66";
      hash = "sha256-Deov6eLNL2pf9MNqe0p5ZHwtpD6+FJDoDA4hMstaGtc=";
    };

    nativeBuildInputs = [jdupes sassc];

    propagatedUserEnvPkgs = [gtk-engine-murrine];

    postPatch = ''
      find -name "*.sh" -print0 | while IFS= read -r -d ''' file; do
        patchShebangs "$file"
      done
    '';

    dontBuild = true;

    installPhase = ''
      runHook preInstall

      mkdir -p $out/share/themes

      ./themes/install.sh \
        --name ${pname} \
        ${toString (map (x: "--theme " + x) accent)} \
        ${lib.optionalString (shade != null) ("--color " + shade)} \
        ${lib.optionalString (size != null) ("--size " + size)} \
        --dest $out/share/themes

      jdupes --quiet --link-soft --recurse $out/share

      runHook postInstall
    '';

    meta = with lib; {
      description = "A GTK theme with the Rosé Pine colour palette.";
      homepage = "https://github.com/Fausto-Korpsvart/Catppuccin-GTK-Theme";
      license = licenses.gpl3Only;
      # maintainers = [ Icy-Thought ];
      platforms = platforms.all;
    };
  }
