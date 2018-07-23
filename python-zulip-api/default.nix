let
  pkgs = import <nixpkgs> {};
  zulip = pkgs.python3Packages.buildPythonPackage rec {
    pname = "zulip";
    version = "0.5.1";
    name = "${pname}-${version}";
    format = "wheel";
    src = pkgs.pythonPackages.fetchPypi {
      inherit format pname version;
      sha256 = "08cm0zlbczdb4p660a7lxr180qhrfwwziy04xx654x7b7l79wpxy";
    };
    propagatedBuildInputs = with pkgs.python3Packages; [ typing requests ];
  };
in pkgs.python3.withPackages (p: [ p.notebook zulip ])
