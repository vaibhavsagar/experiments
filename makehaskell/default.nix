{ ghc-options ? "", main ? "Main.hs", name ? "main", nixpkgs ? import <nixpkgs> {}, packages ? (_: []), src }:
nixpkgs.runCommand name {} ''
  ${nixpkgs.coreutils}/bin/mkdir -pv $out/bin
  cd ${src}
  ${nixpkgs.haskellPackages.ghcWithPackages packages}/bin/ghc ${main} \
    ${ghc-options} \
    -outputdir $(${nixpkgs.coreutils}/bin/mktemp -d) \
    -o $out/bin/${name}
''
