{ lib, self, ... }:

with lib; rec {
  mapModules = dir: fn:
    self.attrs.mapFilterAttrs (n: v: v != null && !(hasPrefix "_" n)) (n: v:
      let path = "${toString dir}/${n}";
      in if v == "directory" && builtins.pathExists "${path}/default.nix" then
        nameValuePair n (fn path)
      else if v == "regular" && n != "default.nix" && hasSuffix ".nix" n then
        nameValuePair (removeSuffix ".nix" n) (fn path)
      else
        nameValuePair "" null) (builtins.readDir dir);

  mapModules' = dir: fn: builtins.attrValues (mapModules dir fn);

  mapModulesRec = dir: fn:
    self.attrs.mapFilterAttrs (n: v: v != null && !(hasPrefix "_" n)) (n: v:
      let path = "${toString dir}/${n}";
      in if v == "directory" then
        nameValuePair n (mapModulesRec path fn)
      else if v == "regular" && n != "default.nix" && hasSuffix ".nix" n then
        nameValuePair (removeSuffix ".nix" n) (fn path)
      else
        nameValuePair "" null) (builtins.readDir dir);

  mapModulesRec' = dir: fn:
    let
      dirs = mapAttrsToList (k: _: "${dir}/${k}")
        (filterAttrs (n: v: v == "directory" && !(hasPrefix "_" n))
          (builtins.readDir dir));
      files = builtins.attrValues (mapModules dir id);
      paths = files ++ builtins.concatLists (map (d: mapModulesRec' d id) dirs);
    in map fn paths;
}
