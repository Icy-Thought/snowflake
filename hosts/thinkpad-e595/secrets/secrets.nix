let
  cryptKey =
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOXkztyXeTnXZ/EztfKnfskgprghDLxO6jczEFnj5DER icy-thought@pm.me";

in { "akkadianVPN.age".publicKeys = [ cryptKey ]; }
