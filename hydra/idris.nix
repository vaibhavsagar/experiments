let idrisPackages = (import <nixpkgs> {}).idrisPackages; in {
    build = idrisPackages.with-packages (with idrisPackages; [ contrib prelude ]);
}
