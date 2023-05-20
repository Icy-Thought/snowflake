let
  inherit (builtins) pathExists readFile;
  keyFile = "/etc/ssh/ed25519_key.pub";
  hostKey =
    if pathExists keyFile
    then readFile keyFile
    else "";
in {
  "closedAI.age".publicKeys = [hostKey];
  "ement.age".publicKeys = [hostKey];
  # "torBylon.age".publicKeys = [ hostKey ];
}
