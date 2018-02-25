let
  inherit (import <nixpkgs> {}) fetchFromGitHub lib;
  overlay = self: super: {
    idrisPackages = self.callPackage <nixpkgs/pkgs/development/idris-modules> {
      idris =
        let
          inherit (super.haskell) lib;
          haskellPackages = super.haskellPackages.override {
            overrides = self: super: {
              binary = lib.dontCheck self.binary_0_8_5_1;
              cheapskate = self.cheapskate_0_1_1;
              idris = lib.overrideCabal (self.callHackage "idris" "1.2.0" {}) (drv: {
                doCheck = false;
                librarySystemDepends = (drv.librarySystemDepends or []) ++ [pkgs.gmp];
                preBuild = ''
                  export LD_LIBRARY_PATH="$PWD/dist/build:$LD_LIBRARY_PATH"
                '';
              });
              megaparsec = self.callHackage "megaparsec" "6.4.0" {};
              parser-combinators = self.callHackage "parser-combinators" "0.4.0" {};
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
  buildInputs = with pkgs; with idrisPackages; [
    (with-packages [ contrib ])
    gcc
    gmp
  ];
} ""
