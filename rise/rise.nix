let
  pkgs = import <nixpkgs> {};
  rise = pkgs.python3Packages.buildPythonPackage rec {
    pname = "rise";
    version = "5.2.0";
    name = "${pname}-${version}";
    src = pkgs.fetchurl {
      url = "mirror://pypi/${builtins.substring 0 1 pname}/${pname}/${name}.tar.gz";
      sha256 = "0yxx1m0fm8zps8cb2pp5hxd3dn57sxghaxxsyhm7qkwd9dwbhkji";
    };
    propagatedBuildInputs = with pkgs.python3Packages; [ notebook ];
  };
in rise
