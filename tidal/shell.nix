let
  pkgs = import <nixpkgs> {};
  tidal-src = pkgs.fetchFromGitHub {
    owner = "tidalcycles";
    repo = "Tidal";
    rev = "0823b39c78893ce1057e2ab0eb4e34d58c498932";
    sha256 = "1f2zdacmi17bckhmv5zpb6v769yl28vrb0836nkfbg7g9haa1c0x";
  };
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      tidal = pkgs.haskell.lib.dontCheck (self.callCabal2nix "tidal" tidal-src {});
    };
  };
in
  pkgs.mkShell {
    buildInputs = [ pkgs.atom (haskellPackages.ghcWithPackages (p: [ p.tidal ])) ];
  }
