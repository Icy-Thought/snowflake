let
  keyFile = "/etc/ssh/ed25519_key.pub";
  hostKey =
    if builtins.pathExists keyFile then builtins.readFile keyFile else "";
in {
  "ClosedAI.age".publicKeys = [ hostKey ];
  "Ement.age".publicKeys = [ hostKey ];
  "tokenGH.age".publicKeys = [ hostKey ];
  "OpenWeatherMap.age".publicKeys = [ hostKey ];
  "mailingQST.age".publicKeys = [ hostKey ];
  "aletheia.age".publicKeys = [ hostKey ];
}
