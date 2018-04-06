let
  inherit (import <nixpkgs> {}) fetchFromGitHub lib;
  versions = lib.mapAttrs
    (_: fetchFromGitHub)
    (builtins.fromJSON (builtins.readFile ./versions.json));
  nixpkgs = import versions.nixpkgs {};
in
nixpkgs.mkShell {
  buildInputs = with nixpkgs.idrisPackages; [
    (with-packages [ base contrib prelude ])
  ];
}
