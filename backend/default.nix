{ nixPkgs ? import <nixpkgs> {}}:
let
  kasti = nixPkgs.haskellPackages.callPackage ./kasti.nix { };
  kasti-minimal = nixPkgs.haskell.lib.justStaticExecutables kasti;

in
  { kasti = kasti;
    kasti-minimal = kasti-minimal;
    env = kasti.env;
  }
