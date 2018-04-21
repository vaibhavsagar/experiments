# { lib
# , buildPythonPackage
# , fetchPypi
# , traitlets
# , glibcLocales
# , mock
# , pytest
# }:
let
  pkgs = import <nixpkgs> {};
  python3 = pkgs.python3;
  ruamel-yaml = python3.pkgs.buildPythonPackage rec {
    pname = "ruamel.yaml";
    version = "0.15.37";
    name = "${pname}-${version}";

    src = python3.pkgs.fetchPypi {
      inherit pname version;
      sha256 = "509842d96fb194f79b57483b76429f8956d8f7ade3cb49d1e5aeb5c5e9ef4918";
    };

    checkInputs = with python3.pkgs; [];
    propagatedBuildInputs = with python3.pkgs; [];
    doCheck = false;

    meta = with pkgs.lib; {
      description = "A python library adding a json log formatter";
      homepage = http://jupyter.org/;
      license = licenses.bsd3;
      maintainers = with maintainers; [ fridh globin ];
    };
  };
  python-json-logger = python3.pkgs.buildPythonPackage rec {
    pname = "python-json-logger";
    version = "0.1.8";
    name = "${pname}-${version}";

    src = python3.pkgs.fetchPypi {
      inherit pname version;
      sha256 = "8205cfe7061715de5cd1b37e3565d5b97d0ac13b30ff3ee612554abb6093d640";
    };

    checkInputs = with python3.pkgs; [];
    propagatedBuildInputs = with python3.pkgs; [];

    meta = with pkgs.lib; {
      description = "A python library adding a json log formatter";
      homepage = http://jupyter.org/;
      license = licenses.bsd3;
      maintainers = with maintainers; [ fridh globin ];
    };
  };
  repo2docker = python3.pkgs.buildPythonPackage rec {
    pname = "jupyter-repo2docker";
    version = "0.5.0";
    name = "${pname}-${version}";

    src = python3.pkgs.fetchPypi {
      inherit pname version;
      sha256 = "8e82bd9a45fce71da3fda8b745ea16b1877b49040a39c6125e267bb47285f7b0";
    };

    checkInputs = with python3.pkgs; [ pytest mock pkgs.glibcLocales ];
    propagatedBuildInputs = with python3.pkgs; [ docker escapism traitlets python-json-logger jinja2 ruamel-yaml ];

    meta = with pkgs.lib; {
      description = "Jupyter core package. A base package on which Jupyter projects rely";
      homepage = http://jupyter.org/;
      license = licenses.bsd3;
      maintainers = with maintainers; [ fridh globin ];
    };
  };
in (python3.withPackages (_: [ repo2docker ])).env
