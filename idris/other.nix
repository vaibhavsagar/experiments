let
  inherit (import <nixpkgs> {}) fetchFromGitHub;
  nixpkgs = import (fetchFromGitHub {
    owner  = "NixOS";
    repo   = "nixpkgs";
    rev    = "41ce36b7eae57c373798701f4b22b0a88e100d88";
    sha256 = "1dy4lwnicr9kvvqdsr18kr4acaklv1b56x9rfj3iaiqwgbhzdiny";
  }) {};
in
nixpkgs.mkShell {
  buildInputs = with nixpkgs.idrisPackages; [
    (with-packages [ base contrib prelude ])
  ];
}
