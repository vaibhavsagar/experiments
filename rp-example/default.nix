let
  # ./updater versions.json reflex-platform
  fetcher = { owner, repo, rev, sha256, ... }: builtins.fetchTarball {
    inherit sha256;
    url = "https://github.com/${owner}/${repo}/tarball/${rev}";
  };
  reflex-platform = fetcher (builtins.fromJSON (builtins.readFile ./versions.json)).reflex-platform;
in (import reflex-platform { system = builtins.currentSystem; }).project ({ pkgs, ... }: {
  useWarp = true;
  packages = {
    rp-example = ./.;
    hamt = ./hamt;
  };
  shells = {
    ghc = ["rp-example" "hamt"];
    ghcjs = ["rp-example" "hamt"];
  };
})
