let
  pkgs = import <nixpkgs> {};
  haskellPackages' = pkgs.haskellPackages.override {
    overrides = self: super: {
      doctemplates = self.callHackage "doctemplates" "0.2.1" {};
      hslua = self.callHackage "hslua" "0.9.1" {};
      pandoc = self.callHackage "pandoc" "2.0.1" {};
      pandoc-types = self.callHackage "pandoc-types" "1.17.2" {};
      skylighting = self.callHackage "skylighting" "0.4.2" {};
    };
  };
in pkgs.runCommand "dummy" {
  buildInputs = [ haskellPackages'.pandoc ];
} ""
