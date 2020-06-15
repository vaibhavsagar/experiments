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
    hlint = pkgs.haskell.lib.overrideCabal (self.callCabal2nix "hlint" (pkgs.fetchFromGitHub {
      owner = "ndmitchell";
      repo = "hlint";
      rev = "c522c198b7c4bccdb804295d9896ba35993b6a2e";
      sha256 = "15lis8pphrq7kp5malwrsg4yyyy99iylf57wn5n5b0ijdm3906wd";
    }) {}) (drv: {
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
