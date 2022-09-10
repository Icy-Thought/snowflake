let
  icyThought = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOXkztyXeTnXZ/EztfKnfskgprghDLxO6jczEFnj5DER";
in {
  "akkadianVPN.age".publicKeys = [icyThought];
  "torBylon.age".publicKeys = [icyThought];
}
