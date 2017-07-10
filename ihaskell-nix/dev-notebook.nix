{ pkgs ? import <nixpkgs> {}, packageSrc ? ./. }:
let
  drv    = pkgs.haskell.lib.dontHaddock (pkgs.haskellPackages.callCabal2nix "this" packageSrc {});
  inputs = with drv;
    (  buildInputs
    ++ nativeBuildInputs
    ++ propagatedBuildInputs
    ++ propagatedNativeBuildInputs
    ++ [ drv ]
    );
in import ./release.nix { packages = (_: inputs); }
