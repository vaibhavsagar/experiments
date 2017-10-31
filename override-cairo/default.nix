let
  overlay = self: super: {
    all-cabal-hashes = super.fetchFromGitHub {
      owner  = "commercialhaskell";
      repo   = "all-cabal-hashes";
      rev    = "a7e38c265b7927a921fbc06b977c1e254cb3142b";
      sha256 = "0n4703mbfdgwnmy5cbzahgg0vmqpin25aafcf30fyl49gbzyjr6g";
    };
  };
in {nixpkgs ? import <nixpkgs> { overlays = [ overlay ]; }}:
let
  lib = nixpkgs.haskell.lib;
  haskellPackages = nixpkgs.haskell.packages.ghc821.override {
    overrides = self: super: {
      gtk2hs-buildtools = super.callHackage "gtk2hs-buildtools" "0.13.3.0" {};
      cairo             = nixpkgs.lib.overrideDerivation super.cairo (drv: {
        src = let
          repo = nixpkgs.fetchFromGitHub {
            owner  = "gtk2hs";
            repo   = "gtk2hs";
            rev    = "f066503df2c6d8d57e06630615d2097741d09d39";
            sha256 = "1drqwz5ry8i9sv34kkywl5hj0p4yffbjgzb5fgpp4dzdgfxl0cqk";
          };
        in "${repo}/cairo";
      });
    };
  };
in (haskellPackages.ghcWithPackages (p: [ p.cairo ]))
