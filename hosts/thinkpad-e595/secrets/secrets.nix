let
  inherit (builtins) pathExists readFile;
  keyFile = "/etc/ssh/ed25519_key.pub";
  hostKey = if pathExists keyFile then readFile keyFile else "";
in {
  "ClosedAI.age".publicKeys = [ hostKey ];
  "Ement.age".publicKeys = [ hostKey ];
  "tokenGH.age".publicKeys = [ hostKey ];
  "OpenWeatherMap.age".publicKeys = [ hostKey ];
  "mailingQST.age".publicKeys = [ hostKey ];
  # "torBylon.age".publicKeys = [ hostKey ];
}
