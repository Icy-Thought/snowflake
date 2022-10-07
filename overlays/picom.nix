final: prev: rec {
  picom = prev.picom.overrideAttrs (old: {
    version = "2022-05-30";
    src = prev.fetchFromGitHub {
      owner = "dccsillag";
      repo = "picom";
      rev = "51b21355696add83f39ccdb8dd82ff5009ba0ae5";
      hash = "sha256-crCwRJd859DCIC0pEerpDqdX2j8ZrNAzVaSSB3mTPN8=";
    };
  });
}
