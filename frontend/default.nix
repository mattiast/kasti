{ nixpkgs ? <nixpkgs>
, config ? {}
}:

with (import nixpkgs config);

let
  srcs = ./elm-srcs.nix;
  versionsDat = ./versions.dat;
in stdenv.mkDerivation {
  name = "elm-app-0.1.0";
  src = ./.;

  buildInputs = [ elmPackages.elm ];

  buildPhase = pkgs.elmPackages.fetchElmDeps {
    elmPackages = import srcs;
    inherit versionsDat;
  };

  installPhase = ''
    elm make Browse.elm --output $out/elm.js
  '';
}

