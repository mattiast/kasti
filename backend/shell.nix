let

  pkgs = import <nixpkgs> { };

in
  pkgs.haskellPackages.callPackage ./kasti.nix { }
