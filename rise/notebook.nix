let
  pkgs = import <nixpkgs> {};
  notebookJson = import ./textfile.nix;
  rise = import ./rise.nix;
  jupyter = pkgs.python3.withPackages (ps: [ ps.jupyter ps.notebook rise ]);
  nbextension = pkgs.runCommand "nbextension" {} ''
    mkdir -p $out/etc/jupyter/nbextensions
    ln -s ${notebookJson} $out/etc/jupyter/nbconfig
    ln -s ${rise}/lib/python3.*/site-packages/rise/static $out/etc/jupyter/nbextensions/rise
  '';
in
pkgs.buildEnv {
  name = "jupyter-with-rise";
  paths = [ jupyter nbextension ];
}
