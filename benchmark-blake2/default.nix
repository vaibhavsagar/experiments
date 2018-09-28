{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = pkgs.haskellPackages.extend (self: super: {
    blake2-patched = self.callPackage ./blake2-patched {};
  });
  drv = haskellPackages.callCabal2nix "benchmark-blake2" (pkgs.lib.cleanSource ./.) {};

in if pkgs.lib.inNixShell then drv.env else drv
