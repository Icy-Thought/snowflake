{
  minimal = {
    path = ./minimal;
    description = "λ well-tailored and configureable NixOS system!";
  };

  # Hosts
  host-desktop = {
    path = ./hosts/desktop;
    description = "A starter hosts/* config for someone's daily driver";
  };

  # host-server = {
  #   path = ./hosts/homeserver;
  #   description = "A starter hosts/* config for a home server config";
  # };

  # Projects
  # TODO: project-rust
  # TODO: project-rust-bevy
}
