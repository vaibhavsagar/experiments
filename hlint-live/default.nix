let
  # ./updater versions.json reflex-platform
  fetcher = { owner, repo, rev, sha256, ... }: builtins.fetchTarball {
    inherit sha256;
    url = "https://github.com/${owner}/${repo}/tarball/${rev}";
  };
  reflex-platform = fetcher (builtins.fromJSON (builtins.readFile ./versions.json)).reflex-platform;
in (import reflex-platform { system = builtins.currentSystem; hieSupport = false; }).project ({ pkgs, ... }: {
  overrides = self: super: {
    extra = self.callHackageDirect {
      pkg = "extra";
      ver = "1.7.1";
      sha256 = "0n23dhsfjjdmprgmdsrrma8q8ys0zc4ab5vhzmiy2f9gkm0jg0pq";
    } {};
    ghc-lib-parser = pkgs.haskell.lib.dontHaddock (self.callHackageDirect {
      pkg = "ghc-lib-parser";
      ver = "8.10.1.20200412";
      sha256 = "05adhjbvkgpx0bwzv1klc2a356d23zqdbj502iapqksirjkk6cqj";
    } {});
    ghc-lib-parser-ex = self.callHackageDirect {
      pkg = "ghc-lib-parser-ex";
      ver = "8.10.0.6";
      sha256 = "043r3j57312ishccq4hwkb4wmh7f98is61kp483xh5sq4r5zqs2x";
    } {};
    hlint = self.callCabal2nix "hlint" (builtins.fetchTarball {
      url = "https://github.com/ndmitchell/hlint/tarball/510277ee0d24d17c9cb33d2832fe089ee8c29631";
      sha256 = "0z74g0xz2rcqqvilmy8dlljj4jhwfs2dkims3nclrryamzak94b6";
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
