{ nixPkgs ? import <nixpkgs> {}}:
let
  kasti = nixPkgs.haskellPackages.callPackage ./kasti.nix rec {
    elm-export = nixPkgs.haskellPackages.callPackage ./elm-export.nix { };
    servant-elm = nixPkgs.haskellPackages.callPackage ./servant-elm.nix { elm-export = elm-export; };
  };
  kasti-minimal = nixPkgs.haskell.lib.justStaticExecutables kasti;

in
  { kasti = kasti;
    kasti-minimal = kasti-minimal;
    env = kasti.env;
  }
