let
  # ./updater versions.json reflex-platform
  fetcher = { owner, repo, rev, sha256, ... }: builtins.fetchTarball {
    inherit sha256;
    url = "https://github.com/${owner}/${repo}/tarball/${rev}";
  };
  reflex-platform = fetcher (builtins.fromJSON (builtins.readFile ./versions.json)).reflex-platform;
in (import reflex-platform { system = builtins.currentSystem; hieSupport = false; }).project ({ pkgs, ... }: {
  overrides = self: super: {
    HsYAML = self.callHackage "HsYAML" "0.2.1.0" {};
    HsYAML-aeson = pkgs.haskell.lib.doJailbreak (self.callHackage "HsYAML-aeson" "0.2.0.0" {});
    extra = self.callHackageDirect {
      pkg = "extra";
      ver = "1.7.3";
      sha256 = "08j4gg2n5cl7ycr943hmyfimgby0xhf5vp8nwrwflg6lrn1s388c";
    } {};
    ghc-lib-parser = pkgs.haskell.lib.dontHaddock (self.callHackageDirect {
      pkg = "ghc-lib-parser";
      ver = "8.10.1.20200518";
      sha256 = "0bidjvilvs9cq413ncwx9qi21y8ymw938mf0j4dwc4jv703q473p";
    } {});
    ghc-lib-parser-ex = self.callHackageDirect {
      pkg = "ghc-lib-parser-ex";
      ver = "8.10.0.14";
      sha256 = "1asyai9pw977n2j28iy6jrlg874s7c0kci2kccg70nc8z401d77d";
    } {};
    hlint = pkgs.haskell.lib.overrideCabal (self.callHackageDirect {
      pkg = "hlint";
      ver = "3.1.5";
      sha256 = "04w2k1y25396vjy1hj4v74mzrfbmjpwnig0dh5sg1pv3mbf3nrq3";
    } {}) (drv: {
      configureFlags = (drv.configureFlags or []) ++ [ "-fhsyaml" ];
      buildDepends = (drv.buildDepends or []) ++ [ self.HsYAML self.HsYAML-aeson ];
      postPatch = (drv.postPatch or "") + ''
        substituteInPlace src/CmdLine.hs --replace \
          "import System.FilePath" \
          "import System.FilePath hiding (isWindows)"
      '';
    });
    hpc = self.callHackage "hpc" "0.6.0.3" { };
    patch = pkgs.haskell.lib.dontCheck super.patch;
    reflex = pkgs.haskell.lib.dontCheck super.reflex;
    reflex-dom-core = pkgs.haskell.lib.dontCheck super.reflex-dom-core;
    reflex-codemirror = self.callCabal2nix "reflex-codemirror" (pkgs.fetchFromGitHub {
      owner = "Atidot";
      repo = "reflex-codemirror";
      rev = "bde5fbc5d33bf9a6a7f2f24c4f39f64a535b6489";
      sha256 = "0zadvfng4h6lsw2vggzxlk7x5nra5g013krfbjwma6dmar7532s6";
    }) {};
    reflex-utils = self.callCabal2nix "reflex-utils" (pkgs.fetchFromGitHub {
      owner = "Atidot";
      repo = "reflex-utils";
      rev = "9ffda7ef04c3d538fdf2ae8d6f3eddcd456de8ab";
      sha256 = "1ljq6ny8abymkslwmlb7x45d1ngapd2zl1g1f8ifhpbb34qx12na";
    }) {};
  };
  useWarp = true;
  withHoogle = false;
  packages = {
    hlint-live = ./hlint-live;
  };
  shells = {
    ghc = ["hlint-live"];
    ghcjs = ["hlint-live"];
  };
})
