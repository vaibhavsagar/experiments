{
  description = "A small reproduction of a bug encountered when using opentelemetry-plugin.";

  inputs.nixpkgs-src.url = "github:NixOS/nixpkgs/release-23.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.opentelemetry-plugin-src.url = "github:MercuryTechnologies/opentelemetry-plugin";

  outputs = {self, nixpkgs-src, flake-utils, opentelemetry-plugin-src, ...}:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlay = sel: sup: {
          haskell = sup.haskell // {
            packages = sup.haskell.packages // {
              ghc96 = sup.haskell.packages.ghc96.override {
                overrides = self: super: {
                  opentelemetry-plugin = self.callCabal2nix "opentelemetry-plugin" opentelemetry-plugin-src {};
                };
              };
            };
          };
        };
        nixpkgs = import nixpkgs-src { inherit system; overlays = [ overlay ]; };
        opentelemetry-plugin-bug = nixpkgs.haskell.packages.ghc96.callCabal2nix "opentelemetry-plugin-bug" ./. {};
      in {
        defaultPackage = opentelemetry-plugin-bug;
      });
}
