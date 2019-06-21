let
  pkgs = import <nixpkgs> {};
in pkgs.buildFHSUserEnv {
  name = "fhs";
  targetPkgs = pkgs: [
    pkgs.binutils
    pkgs.blas
    pkgs.cairo.dev
    pkgs.file
    pkgs.gcc
    pkgs.glib.dev
    pkgs.gmp
    pkgs.gnumake
    pkgs.haskellPackages.stack
    pkgs.iana-etc
    pkgs.liblapack
    pkgs.pango.dev
    pkgs.perl
    pkgs.pkgconfig
    (pkgs.python3.withPackages (ps: [ ps.jupyter ps.notebook ]))
    pkgs.ncurses
    pkgs.zeromq
    pkgs.zlib.dev
  ];
  profile = ''
    export C_INCLUDE_PATH=/usr/include:$C_INCLUDE_PATH
  '';
}
