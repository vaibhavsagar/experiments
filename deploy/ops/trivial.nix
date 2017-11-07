{
  network.description = "Web server";

  webserver = { config, pkgs, ... }: {
    imports = [ ./blank-me-up/service.nix ];
    services.blank-me-up.enable = true;
  };
}
