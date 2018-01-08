let
  inherit (import <nixpkgs> {}) fetchFromGitHub lib;
  overlay = self: super: {
    all-cabal-hashes = super.fetchFromGitHub {
      owner = "commercialhaskell";
      repo  = "all-cabal-hashes";
      rev    = "8ed17011c07ce97b5b5ed5a7481f20043d0d00cd";
      sha256 = "0dmmv1qfsk0lx0cgi7bq4v12zdqm8ylxkhqnfqjz45wpwza2bm45";
    };
    idrisPackages = super.callPackage <nixpkgs/pkgs/development/idris-modules> {
      idris =
        let
          inherit (super.haskell) lib;
          haskellPackages = super.haskellPackages.override {
            overrides = self: super: {
              binary = lib.dontCheck self.binary_0_8_5_1;
              cheapskate = self.cheapskate_0_1_1;
              idris = lib.overrideCabal (self.callHackage "idris" "1.2.0" {}) (drv: {
                doCheck = false; # I don't want to install Node :(
                librarySystemDepends = (drv.librarySystemDepends or []) ++ [pkgs.gmp];
                preBuild = ''
                  export LD_LIBRARY_PATH="$PWD/dist/build:$LD_LIBRARY_PATH"
                '';
              });
              megaparsec = lib.doJailbreak (self.callHackage "megaparsec" "6.3.0" {});
              parsers = lib.dontCheck super.parsers;
              semigroupoids = lib.dontCheck super.semigroupoids;
            };
          };
        in
          haskellPackages.idris;
    };
  };
  versions = lib.mapAttrs
    (_: fetchFromGitHub)
    (builtins.fromJSON (builtins.readFile ./versions.json));
  pkgs = import versions.nixpkgs { overlays = [ overlay ]; };
in
pkgs.runCommand "dummy" {
  buildInputs = with pkgs; [
    (with idrisPackages; with-packages [ contrib ])
    gcc
    gmp
  ];
} ""
