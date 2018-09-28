{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = pkgs.haskellPackages.extend (self: super: {
    blake2-patched = self.callCabal2nix "blake2-patched" (pkgs.fetchFromGitHub {
      owner = "vaibhavsagar";
      repo = "blake2";
      rev = "86558533c2ec674f27a3c634d93b73a2623c11e2";
      sha256 = "1pa3kjla8zcbl021h68fd79dyhgv1rz43s25k6pnf510pma5z8lb";
    }) {};
  });
  drv = haskellPackages.callCabal2nix "benchmark-blake2" (pkgs.lib.cleanSource ./.) {};

in if pkgs.lib.inNixShell then drv.env else drv
