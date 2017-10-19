{ pkgs ? import <nixpkgs> {} }:

pkgs.runCommand "dummy" {
  buildInputs = with pkgs; [
    idrisPackages.idris
    gcc
    gmp
  ];
} ""
