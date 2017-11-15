{ config, lib, pkgs, ... }:

let
  cfg = config.services.blank-me-up;
  blank-me-up = pkgs.callPackage ./default.nix { nixpkgs = pkgs; };
in {
  options.services.blank-me-up.enable = lib.mkEnableOption "Blank Me Up";

  config = lib.mkIf cfg.enable {
    networking.firewall.allowedTCPPorts = [ 3000 ];

    systemd.services.blank-me-up = {
      description = "Blank Me Up";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        ExecStart = "${blank-me-up}/bin/blank-me-up";
        Restart = "always";
        KillMode = "process";
      };
    };
  };
}
