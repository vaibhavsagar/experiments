let
  pkgs = import <nixpkgs> {};
in pkgs.writeText "notebook.json" (builtins.toJSON {
  load_extensions = {
    "rise/main" = true;
  };
})
