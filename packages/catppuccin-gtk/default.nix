{
  lib,
  stdenv,
  fetchFromGitHub,
  gtk-engine-murrine,
  jdupes,
  sassc,
  accent ? ["default"],
  shade ? "dark",
  size ? "standard",
}: let
  validAccents = ["default" "purple" "pink" "red" "orange" "yellow" "green" "teal" "grey"];
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
    version = "0-unstable-2024-06-27";

    src = fetchFromGitHub {
      owner = "Fausto-Korpsvart";
      repo = "Catppuccin-GTK-Theme";
      rev = "0bd2869e7f0fdb36c720a4fb873d4fed361b0606";
      hash = "sha256-oFVsYrJ27hYGY+x9+Z4SxVCp3w6PiLYTZaxmGhnpVHQ=";
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
      description = "GTK Theme with Catppuccin colour scheme";
      homepage = "https://github.com/Fausto-Korpsvart/Catppuccin-GTK-Theme";
      license = licenses.gpl3Only;
      # maintainers = with maintainers; [ Icy-Thought ];
      platforms = platforms.all;
    };
  }
