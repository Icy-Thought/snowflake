{
  stdenv,
  lib,
  nerd-font-patcher,
  iosevka ?
    iosevka.override {
      privateBuildPlan = {
        family = "Iosevka Custom";
        spacing = "normal";
        serifs = "sans";
        no-cv-ss = true;
        no-litigation = false;
      };
      set = "custom";
    },
}:
stdenv.mkDerivation {
  pname = "NF-Iosevka";
  version = iosevka.version;

  nativeBuildInputs = [nerd-font-patcher iosevka];

  phases = ["installPhase"];

  preInstall = ''
    mkdir -p $out/share/fonts/truetype && cd "$_"
  '';

  installPhase = ''
    runHook preInstall

    find ${iosevka}/share/fonts/truetype \
      -name \*.ttf \
      -exec ${nerd-font-patcher}/bin/nerd-font-patcher --complete --quiet --no-progressbars {} \; \
      -exec ${nerd-font-patcher}/bin/nerd-font-patcher --complete --use-single-width-glyphs --adjust-line-height --quiet --no-progressbars {} \;
  '';

  meta = with lib; {
    description = "Versatile typeface for code, from code";
    homepage = "https://be5invis.github.io/Iosevka";
    license = licenses.ofl;
    maintainers = with maintainers; [];
    platforms = platforms.all;
  };
}
