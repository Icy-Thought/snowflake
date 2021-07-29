{ config, lib, pkgs, ... }:

let
  buildTheme = { name, version, src, themeIni ? [ ] }:
    pkgs.stdenv.mkDerivation rec {
      pname = "sddm-theme-${name}";
      inherit version src;

      buildCommand = ''
        dir=$out/share/sddm/themes/${name}
        doc=$out/share/doc/${pname}

        mkdir -p $dir $doc
        if [ -d $src/${name} ]; then
          srcDir=$src/${name}
        else
          srcDir=$src
        fi
        cp -r $srcDir/* $dir/
        for f in $dir/{AUTHORS,COPYING,LICENSE,README,*.md,*.txt}; do
          test -f $f && mv $f $doc/
        done
        chmod 444 $dir/theme.conf

        ${lib.concatMapStringsSep "\n" (e: ''
          ${pkgs.crudini}/bin/crudini --set --inplace $dir/theme.conf \
            "${e.section}" "${e.key}" "${e.value}"
        '') themeIni}
      '';
    };

  customTheme = builtins.isAttrs theme;
  theme = themes.aerial;

  themeName = if customTheme then theme.pkg.name else theme;

  packages =
    if customTheme then [ (buildTheme theme.pkg) ] ++ theme.deps else [ ];

  themes.aerial = {
    pkg = rec {
      name = "aerial";
      version = "20210728";
      src = pkgs.fetchFromGitHub {
        owner = "3ximus";
        repo = "${name}-sddm-theme";
        rev = "2fa0a4024bab60b0ba40de274880e0c1aa6eca59";
        sha256 = "jaGQaClD7Hk4eWh+rMX8ZtcGDzb9aCu+NX5gzJ1JXQg=";
      };
    };
    deps = with pkgs; [ qt5.qtmultimedia ];
  };

in {
  environment.systemPackages = packages;

  services.xserver.displayManager.sddm.theme = themeName;
}
