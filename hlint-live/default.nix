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
      ver = "1.7.1";
      sha256 = "0n23dhsfjjdmprgmdsrrma8q8ys0zc4ab5vhzmiy2f9gkm0jg0pq";
    } {};
    ghc-lib-parser = pkgs.haskell.lib.dontHaddock (self.callCabal2nix "ghc-lib-parser" ./ghc-lib-parser-8.10.1.20200412 {});
    ghc-lib-parser-ex = self.callHackageDirect {
      pkg = "ghc-lib-parser-ex";
      ver = "8.10.0.6";
      sha256 = "043r3j57312ishccq4hwkb4wmh7f98is61kp483xh5sq4r5zqs2x";
    } {};
    hlint = self.callCabal2nix "hlint" ./hlint-3.1.1 {};
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
