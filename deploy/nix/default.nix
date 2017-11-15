{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, scotty, stdenv }:
      mkDerivation {
        pname = "blank-me-up";
        version = "0.1.0.0";
        src = ../app;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [ base scotty ];
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
