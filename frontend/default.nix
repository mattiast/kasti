{ nixpkgs ? <nixpkgs>
, config ? {}
}:

with (import nixpkgs config);

let
  srcs = ./elm-srcs.nix;
  versionsDat = ./versions.dat;
in stdenv.mkDerivation {
  name = "kasti-frontend";
  src = ./.;

  buildInputs = [ elmPackages.elm ];

  buildPhase = pkgs.elmPackages.fetchElmDeps {
    elmPackages = import srcs;
    inherit versionsDat;
  };

  installPhase = ''
    elm make src/Browse.elm --output $out/elm.js
  '';
}

