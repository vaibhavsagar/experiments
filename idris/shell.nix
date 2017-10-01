{ pkgs ? import <nixpkgs> {} }:

pkgs.runCommand "dummy" {
  buildInputs = with pkgs; [
    haskellPackages.idris
    gcc
    gmp
  ];
} ""
