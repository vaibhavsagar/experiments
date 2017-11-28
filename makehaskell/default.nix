{ main ? "Main.hs", nixpkgs ? import <nixpkgs> {}, packages ? (_: []), name ? "main", src }:
nixpkgs.runCommand name {} ''
  ${nixpkgs.coreutils}/bin/mkdir -pv $out/bin
  cd ${src}
  ${nixpkgs.haskellPackages.ghcWithPackages packages}/bin/ghc ${main} \
    -outputdir $(${nixpkgs.coreutils}/bin/mktemp -d) \
    -o $out/bin/${name}
''
