let
  pkgs = import <nixpkgs> {};
in pkgs.writeTextDir "notebook.json" (builtins.toJSON {
  load_extensions = {
    "rise/main" = true;
  };
})
