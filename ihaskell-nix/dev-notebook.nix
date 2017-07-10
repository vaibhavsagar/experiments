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
  isHaskellPkg = p: (p ? pname) && (p ? version) && (p ? env);
  haskellInputs = pkgs.stdenv.lib.filter isHaskellPkg inputs;
in import ./release.nix { packages = (_: haskellInputs); }
