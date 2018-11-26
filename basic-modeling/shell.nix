let
  pkgs = import <nixpkgs> {};
  gecode-configuration = pkgs.writeText "gecode-configuration.msc" (builtins.toJSON  {
    description = "Gecode FlatZinc executable";
    executable = "fzn-gecode";
    id = "org.gecode.gecode";
    isGUIApplication = false;
    name = "Gecode";
    needsMznExecutable = false;
    needsSolns2Out = true;
    needsStdlibDir = false;
    stdFlags = [ "-a" "-f" "-n" "-p" "-r" "-s" "-t" ];
    supportsFzn = true;
    supportsMzn = false;
    tags = [ "cp" "int" "float" "set" "restart" ];
    version = "6.0.0";
  });
  gecode-configuration-dir = pkgs.runCommand "gecode-configuration-dir" {} ''
    mkdir -p $out
    cp ${gecode-configuration} $out/
  '';
in
  pkgs.mkShell {
    buildInputs = with pkgs; [ python3 minizinc gecode ];
    MZN_SOLVER_PATH="${gecode-configuration-dir}";
}
