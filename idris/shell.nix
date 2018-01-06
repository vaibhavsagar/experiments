let
  inherit (import <nixpkgs> {}) fetchFromGitHub lib;
  versions = lib.mapAttrs
    (_: fetchFromGitHub)
    (builtins.fromJSON (builtins.readFile ./versions.json));
  pkgs = import versions.nixpkgs {};
in
pkgs.runCommand "dummy" {
  buildInputs = with pkgs; [
    (with idrisPackages; with-packages [ contrib ])
    gcc
    gmp
  ];
} ""
