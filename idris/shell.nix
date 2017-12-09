{ pkgs ? import <nixpkgs> {} }:

pkgs.runCommand "dummy" {
  buildInputs = with pkgs; [
    (with idrisPackages; with-packages [ contrib ])
    gcc
    gmp
  ];
} ""
