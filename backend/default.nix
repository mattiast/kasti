let
  config = rec {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackgesOld: rec {
          kasti-minimal = pkgs.haskell.lib.justStaticExecutables kasti;
          kasti = haskellPackagesNew.callPackage ./kasti.nix { };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { kasti = pkgs.haskellPackages.kasti;
    kasti-minimal = pkgs.haskellPackages.kasti-minimal;
    env = pkgs.haskellPackages.kasti.env;
  }
