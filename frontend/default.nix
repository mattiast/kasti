{ nixpkgs ? import <nixpkgs> {}
}:

with nixpkgs;

let
  srcs = ./elm-srcs.nix;
  versionsDat = ./versions.dat;
in stdenv.mkDerivation {
  name = "kasti-frontend.js";
  src = ./.;

  buildInputs = [ elmPackages.elm ];

  buildPhase = elmPackages.fetchElmDeps {
    elmPackages = import srcs;
    inherit versionsDat;
  };

  installPhase = ''
    ${elmPackages.elm}/bin/elm make src/Browse.elm --output $out
  '';
}
