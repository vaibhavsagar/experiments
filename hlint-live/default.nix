let
  # ./updater versions.json reflex-platform
  fetcher = { owner, repo, rev, sha256, ... }: builtins.fetchTarball {
    inherit sha256;
    url = "https://github.com/${owner}/${repo}/tarball/${rev}";
  };
  reflex-platform = fetcher (builtins.fromJSON (builtins.readFile ./versions.json)).reflex-platform;
in (import reflex-platform { system = builtins.currentSystem; }).project ({ pkgs, ... }: {
  overrides = self: super: {
    hlint = pkgs.haskell.lib.overrideCabal super.hlint (drv: {
      configureFlags = (drv.configureFlags or []) ++ [ "-fhsyaml" ];
      buildDepends = (drv.buildDepends or []) ++ [ self.HsYAML self.HsYAML-aeson ];
      postPatch = (drv.postPatch or "") + ''
        substituteInPlace src/CmdLine.hs --replace \
          "import System.FilePath" \
          "import System.FilePath hiding (isWindows)"
      '';
    });
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
