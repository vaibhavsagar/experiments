{ pkgs ? import <nixpkgs> {}, packageSrc ? ./. }:
let
  drv    = pkgs.haskellPackages.callCabal2nix "this" packageSrc {};
  inputs = with drv;
    (  buildInputs
    ++ nativeBuildInputs
    ++ propagatedBuildInputs
    ++ propagatedNativeBuildInputs
    );
in import ./release.nix { packages = (_: inputs); }
