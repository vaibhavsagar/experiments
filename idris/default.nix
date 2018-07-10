let
  fetcher = { owner, repo, rev, sha256 }: builtins.fetchTarball {
    inherit sha256;
    url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
  };
  overlay = self: super: {
    all-cabal-hashes = super.fetchurl {
      url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/532d9c821a36f220c928be40465a6ace52bc3818.tar.gz";
      sha256 = "1yqn87r75cdf45wkbfa5vqxvsaxqsmypwjl4pw5w1g8qfrdilr18";
    };

    idrisPackages = super.idrisPackages.override {
      idris-no-deps = let
        haskellPackages = self.haskellPackages.extend (self': super': {
          fingertree = self'.callHackage "fingertree" "0.1.4.1" {};
          idris = self.haskell.lib.dontCheck (self'.callHackage "idris" "1.3.0" {});
        });
      in haskellPackages.idris;
    };
  };
  nixpkgs = import (fetcher (builtins.fromJSON (builtins.readFile ./versions.json)).nixpkgs) { overlays = [ overlay ]; };
  idrisWithPackages =
    with nixpkgs.idrisPackages; (with-packages [ base contrib prelude ]);

in
  if nixpkgs.lib.inNixShell
  then nixpkgs.mkShell { buildInputs =  [ idrisWithPackages ]; }
  else idrisWithPackages
