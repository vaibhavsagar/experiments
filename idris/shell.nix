let
  fetcher = { owner, repo, rev, sha256 }: builtins.fetchTarball {
    inherit sha256;
    url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
  };
  nixpkgs = import (fetcher (builtins.fromJSON (builtins.readFile ./versions.json)).nixpkgs) {};
in
nixpkgs.mkShell {
  buildInputs = with nixpkgs.idrisPackages; [
    (with-packages [ base contrib prelude ])
  ];
}
