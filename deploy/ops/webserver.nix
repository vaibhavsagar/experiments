{ ... }: {
  imports = [ ../nix/service.nix ];
  services.blank-me-up.enable = true;
}
