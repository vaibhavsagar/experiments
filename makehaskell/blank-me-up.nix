(import ./default.nix) {
  ghc-options = "-O2";
  packages = (p: [ p.scotty ]);
  name = "blank-me-up";
  src = ../deploy/app;
}
