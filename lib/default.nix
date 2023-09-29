{
  inputs,
  lib,
  pkgs,
  ...
}: let
  inherit (lib.attrsets) attrValues;
  inherit (lib.fixedPoints) makeExtensible;
  inherit (lib.lists) foldr;
  inherit (modules) mapModules;

  modules = import ./modules.nix {
    inherit lib;
    self.attrs = import ./attrs.nix {
      inherit lib;
      self = {};
    };
  };
  mylib =
    makeExtensible (self:
      mapModules ./. (file: import file {inherit self lib pkgs inputs;}));
in
  mylib.extend (self: super: foldr (a: b: a // b) {} (attrValues super))
