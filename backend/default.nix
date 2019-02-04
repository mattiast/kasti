let
  config = rec {
    packageOverrides = pkgs: rec {
      docker-container-small = pkgs.dockerTools.buildImage {
        name = "kasti-container";
        config.Cmd = [ "${haskellPackages.kasti-minimal}/bin/kasti-server" ];
      };

      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackgesOld: rec {
          kasti-minimal =
              ( pkgs.haskell.lib.justStaticExecutables
                  ( haskellPackagesNew.callPackage ./kasti.nix { }
                  )
              );
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { kasti = pkgs.haskellPackages.kasti-minimal;
    docker-container-small = pkgs.docker-container-small;
  }
