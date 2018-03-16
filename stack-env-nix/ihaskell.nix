let
  pkgs = import <nixpkgs> {};
in pkgs.buildFHSUserEnv {
  name = "fhs";
  targetPkgs = pkgs: [
    pkgs.gcc
    pkgs.gmp
    pkgs.gnumake
    pkgs.haskellPackages.stack
    pkgs.iana-etc
    pkgs.perl
    pkgs.pkgconfig
    (pkgs.python3.withPackages (ps: [ ps.jupyter ps.notebook ]))
    pkgs.ncurses
    pkgs.zlib.dev
    pkgs.zlib.out
    pkgs.zeromq
  ];
  profile = ''
    export C_INCLUDE_PATH=/usr/include:$C_INClUDE_PATH
  '';
}
