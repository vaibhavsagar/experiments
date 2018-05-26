let
  inherit (import <nixpkgs> {}) fetchFromGitHub;
  nixpkgs = fetchFromGitHub {
    owner  = "NixOS";
    repo   = "nixpkgs-channels";
    rev    = "2f6440eb09b7e6e3322720ac91ce7e2cdeb413f9";
    sha256 = "0vb7ikjscrp2rw0dfw6pilxqpjm50l5qg2x2mn1vfh93dkl2aan7";
  };
  pkgs = import nixpkgs {};
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      pandoc = self.callHackage "pandoc" "2.2" {};
      pandoc-types = self.callHackage "pandoc-types" "1.17.4.2" {};
    };
  };
in pkgs.runCommand "dummy" {
  buildInputs = [ haskellPackages.pandoc ];
} ""
