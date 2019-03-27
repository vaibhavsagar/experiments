let
  pkgs = import <nixpkgs> {};
in pkgs.callPackage ./hugs.nix {}
