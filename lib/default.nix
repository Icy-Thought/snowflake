{ inputs, lib, pkgs, ... }:

with lib;

let
  modules = import ./modules.nix {
    inherit lib;
    self.attrs = import ./attrs.nix {
      inherit lib;
      self = { };
    };
  };
  mylib = makeExtensible (self:
    modules.mapModules ./.
    (file: import file { inherit self lib pkgs inputs; }));
in mylib.extend (self: super: foldr (a: b: a // b) { } (attrValues super))
