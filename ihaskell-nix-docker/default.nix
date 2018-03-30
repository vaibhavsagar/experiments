let
  nixpkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs-channels/archive/d16ffa1509008b53891412b095cea259dd73d869.tar.gz";
    sha256 = "0ir4cbld2c5h31aiap9jhj7mpl0nw2yvr9lmw6zn8ndidniv8ki3";
  }) {};
  ihaskell = builtins.fetchTarball {
    url = "https://github.com/gibiansky/IHaskell/archive/2d43d305d1b0e50ed8dcdcc6b959ef3f3d9cdc0e.tar.gz";
    sha256 = "0y1a13irikyziva5b4fh3vhmncrh0fdn4dj7k2ar65zj41yd9l13";
  };
in import "${ihaskell}/release.nix" {
  inherit nixpkgs;
}
