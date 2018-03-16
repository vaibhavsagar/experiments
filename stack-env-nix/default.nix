let
  pkgs = import <nixpkgs> {};
in pkgs.buildFHSUserEnv {
  name = "fhs";
  targetPkgs = pkgs: [
    pkgs.iana-etc
    pkgs.haskellPackages.stack
    pkgs.gcc
    pkgs.gmp
    pkgs.gnumake
    pkgs.perl
    pkgs.ncurses
    pkgs.zlib.dev
  ];
}
