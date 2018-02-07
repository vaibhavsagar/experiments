let
  inherit (import <nixpkgs> {}) fetchFromGitHub;
  nixpkgs = import (fetchFromGitHub {
    owner  = "NixOS";
    repo   = "nixpkgs";
    rev    = "a9268dd694c27700c78b2f14136493a935b738aa";
    sha256 = "01wfrirg5yxrx48n4g7fp9yazk54nja1yvf5j9si4pskh47xs3m9";
  }) {};
  haskellPackages = nixpkgs.haskell.packages.ghc841.extend (self: super: {
    haskell-src-exts  = self.callCabal2nix "haskell-src-exts" (nixpkgs.fetchFromGitHub {
      owner  = "haskell-suite";
      repo   = "haskell-src-exts";
      rev    = "935f6f0915e89c314b686bdbdc6980c72335ba3c";
      sha256 = "1v3c1bd5q07qncqfbikvs8h3r4dr500blm5xv3b4jqqv69f0iam9";
    }) {};
  });
in
  haskellPackages.ghcWithPackages (p: [ p.haskell-src-exts ])
