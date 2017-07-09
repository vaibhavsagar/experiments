{ pkgs ? import <nixpkgs> {}, packageSrc ? ./. }:
let
  drv    = pkgs.haskellPackages.callCabal2nix "this" packageSrc {};
  inputs = with drv; ( propagatedBuildInputs ++ propagatedNativeBuildInputs );
in import ./release.nix { packages = (_: inputs); }
