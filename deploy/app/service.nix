{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.deploy;
in {
  options.services.deploy.enable = mkEnableOption "Deployment";

  config = mkIf cfg.enable {
    systemd.services.deploy
  };
}
