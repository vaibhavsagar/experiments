{ pkgs ? import <nixpkgs> {} }:

pkgs.runCommand "dummy" {
  buildInputs = with pkgs; [
    (idrisPackages.with-packages (with idrisPackages; [ contrib prelude ]))
    gcc
    gmp
  ];
} ""
