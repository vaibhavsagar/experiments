{
  network.description = "Web server";

  webserver = { config, pkgs, ... }: {
    imports = [ ../nix/service.nix ];
    services.blank-me-up.enable = true;
  };
}
