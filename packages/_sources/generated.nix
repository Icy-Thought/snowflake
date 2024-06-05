# This file was generated by nvfetcher, please do not modify it manually.
{ fetchgit, fetchurl, fetchFromGitHub, dockerTools }:
{
  nx-dark-reader = {
    pname = "nx-dark-reader";
    version = "57dc1a9482a81d7c15b00c4b9d647bf1b129d8e4";
    src = fetchFromGitHub {
      owner = "aartaka";
      repo = "nx-dark-reader";
      rev = "57dc1a9482a81d7c15b00c4b9d647bf1b129d8e4";
      fetchSubmodules = false;
      sha256 = "sha256-CKJGCk7ATd0naFhill5rrvay6V2JE4Bstu4tFpaAjB0=";
    };
    date = "2023-06-20";
  };
  nx-search-engines = {
    pname = "nx-search-engines";
    version = "f81f47df82f8e322f0a8919d05fc513297095a0b";
    src = fetchFromGitHub {
      owner = "aartaka";
      repo = "nx-search-engines";
      rev = "f81f47df82f8e322f0a8919d05fc513297095a0b";
      fetchSubmodules = false;
      sha256 = "sha256-Q+6B3lDzlhfHnS306yLh5LpxdIXESdH/Y2D3GJodbXs=";
    };
    date = "2023-06-07";
  };
  pantalaimon = {
    pname = "pantalaimon";
    version = "0ae174364f5746f1c4495b2db75cf4d8b1f613d0";
    src = fetchgit {
      url = "https://gitlab.com/greenbeast/pantalaimon.git";
      rev = "0ae174364f5746f1c4495b2db75cf4d8b1f613d0";
      fetchSubmodules = false;
      deepClone = false;
      leaveDotGit = false;
      sha256 = "sha256-haXi2eKygE7wbbp+Dxu03T0whY6bzpDcVF/W97vKmlg=";
    };
    date = "2024-06-04";
  };
  picom = {
    pname = "picom";
    version = "c2cb4df94ae851de71d16051903ca6247d926c6b";
    src = fetchFromGitHub {
      owner = "yshui";
      repo = "picom";
      rev = "c2cb4df94ae851de71d16051903ca6247d926c6b";
      fetchSubmodules = false;
      sha256 = "sha256-Sj6WevYD8XLMMuTfNiOuAkaSvHLmSQ+vdj+cmEsfxgU=";
    };
    date = "2024-06-04";
  };
  xonsh-cmd-durations = {
    pname = "xonsh-cmd-durations";
    version = "c4101bb94ca54507e0af588963ea46424c10e090";
    src = fetchFromGitHub {
      owner = "jnoortheen";
      repo = "xontrib-cmd-durations";
      rev = "c4101bb94ca54507e0af588963ea46424c10e090";
      fetchSubmodules = false;
      sha256 = "sha256-qFIjXBLyNqGnrslMvhqKpTvJDT79yWdHkDvS6JebVUk=";
    };
    date = "2024-05-17";
  };
  xonsh-direnv = {
    pname = "xonsh-direnv";
    version = "3bea5847b9459c5799c64966ec85e624d0be69b9";
    src = fetchFromGitHub {
      owner = "74th";
      repo = "xonsh-direnv";
      rev = "3bea5847b9459c5799c64966ec85e624d0be69b9";
      fetchSubmodules = false;
      sha256 = "sha256-h56Gx/MMCW4L6nGwLAhBkiR7bX+qfFk80LEsJMiDtjQ=";
    };
    date = "2022-06-19";
  };
  xonsh-fzf-widgets = {
    pname = "xonsh-fzf-widgets";
    version = "8af47d1d684a14eb776485ef6f5c30c8e6807f60";
    src = fetchFromGitHub {
      owner = "laloch";
      repo = "xontrib-fzf-widgets";
      rev = "8af47d1d684a14eb776485ef6f5c30c8e6807f60";
      fetchSubmodules = false;
      sha256 = "sha256-lz0oiQSLCIQbnoQUi+NJwX82SbUvXJ+3dEsSbOb20q4=";
    };
    date = "2020-10-16";
  };
  xonsh-hist-navigator = {
    pname = "xonsh-hist-navigator";
    version = "90504cc163d1f1f36f90fbf2f0190b03b89b798e";
    src = fetchFromGitHub {
      owner = "jnoortheen";
      repo = "xontrib-hist-navigator";
      rev = "90504cc163d1f1f36f90fbf2f0190b03b89b798e";
      fetchSubmodules = false;
      sha256 = "sha256-yosOwmNBNGAW3Ut4z6GKxumWw80VEPWoOvVmKae/fvA=";
    };
    date = "2023-03-16";
  };
  xontrib-output-search = {
    pname = "xontrib-output-search";
    version = "5f4cefe1b15b45041179ac5a92abe988f6d8a6d7";
    src = fetchFromGitHub {
      owner = "anki-code";
      repo = "xontrib-output-search";
      rev = "5f4cefe1b15b45041179ac5a92abe988f6d8a6d7";
      fetchSubmodules = false;
      sha256 = "sha256-oG7VopEYKCZpJ7N2caoJ3XYeFhObzRqksOkhcaHwQn8=";
    };
    date = "2024-03-06";
  };
  xontrib-readable-traceback = {
    pname = "xontrib-readable-traceback";
    version = "d87ddfae258cc928abdff364fac5a108d49f2308";
    src = fetchFromGitHub {
      owner = "vaaaaanquish";
      repo = "xontrib-readable-traceback";
      rev = "d87ddfae258cc928abdff364fac5a108d49f2308";
      fetchSubmodules = false;
      sha256 = "sha256-ek+GTWGUpm2b6lBw/7n4W46W2R0Gy6JxqWoLuQilCXQ=";
    };
    date = "2022-09-07";
  };
  xontrib-schedule = {
    pname = "xontrib-schedule";
    version = "02c4d4f237451fce39b45545d69ab9ea67565b45";
    src = fetchFromGitHub {
      owner = "AstraLuma";
      repo = "xontrib-schedule";
      rev = "02c4d4f237451fce39b45545d69ab9ea67565b45";
      fetchSubmodules = false;
      sha256 = "sha256-Ahog47+Sq9xv03s0kPRm2wEOT989qI3GMaAzlnz/wS4=";
    };
    date = "2021-07-21";
  };
}
