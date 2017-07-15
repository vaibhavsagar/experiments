let
  pkgs = import <nixpkgs> {};
  bash_kernel = pkgs.python3Packages.buildPythonPackage rec {
    name = "bash_kernel-${version}";
    version = "0.6";

    format = "wheel";

    src = pkgs.fetchurl {
      url = "https://pypi.python.org/packages/26/ee/25d12bb777ed56c452a036c46e8360481fca0d9cc9747a337b23deacd821/bash_kernel-0.6-py2.py3-none-any.whl";
      sha256 = "1ca5g8yl40q9m3pkz8ac74s3mhb0vxp1aqpq2dxdr40vl57n4jba";
    };

    propagatedBuildInputs = with pkgs.python3Packages; [ pexpect ];

    meta = {
      homepage = "https://github.com/takluyver/bash_kernel";
      description = "";
    };
  };
  jupyter = pkgs.python3.buildEnv.override {
    extraLibs = with pkgs.python3Packages; [ ipython ipykernel jupyter_client notebook bash_kernel ];
  };
  bashSh = pkgs.writeScriptBin "bash-notebook" ''
    #! ${pkgs.stdenv.shell}
    export PATH="${pkgs.stdenv.lib.makeBinPath [ jupyter bash_kernel ]}"
    ${pkgs.python3.interpreter} -m bash_kernel.install && ${jupyter}/bin/jupyter notebook
  '';
in
pkgs.buildEnv {
  name = "bash-notebook-env";
  paths = [ jupyter bash_kernel ];
  postBuild = ''
    . "${pkgs.makeWrapper}/nix-support/setup-hook"
    ln -s ${bashSh}/bin/bash-notebook $out/bin/.
    for prg in $out/bin"/"*;do
      wrapProgram $prg --set PYTHONPATH "$(echo ${jupyter}/lib/*/site-packages)"
    done
  '';
}
