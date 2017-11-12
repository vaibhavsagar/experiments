{ pkgs ? import <nixpkgs> {}, packages ? (_: []), name ? "main", src }:
pkgs.runCommand name { inherit src; } ''
  ${pkgs.coreutils}/bin/mkdir -pv $out/bin
  cd $src
  ${pkgs.haskellPackages.ghcWithPackages packages}/bin/ghc --make Main.hs \
    -outputdir $(${pkgs.coreutils}/bin/mktemp -d) \
    -o $out/bin/${name}
''
