{ pkgs ? import <nixpkgs> {}, packages ? (_: []), name ? "main", src }:
pkgs.runCommand name {
  buildInputs = [ (pkgs.haskellPackages.ghcWithPackages packages) ];
} ''
  mkdir -pv $out/bin
  ghc --make ${src}/Main.hs -outputdir $(mktemp -d) -o $out/bin/${name}
''
