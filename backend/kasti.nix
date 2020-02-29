{ mkDerivation, aeson, async, base, bytestring, containers
, directory, elm-bridge, feed, filepath, hpack, http-types, lens
, lens-aeson, lifted-async, mtl, optparse-applicative, parallel
, postgresql-simple, process, random, resource-pool, scotty
, servant, servant-elm, servant-server, stdenv, stm, text, time
, transformers, unix, unliftio, vector, wai, warp, wreq
}:
mkDerivation {
  pname = "kasti";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async base bytestring containers directory elm-bridge feed
    filepath http-types lens lens-aeson lifted-async mtl parallel
    postgresql-simple process random resource-pool scotty servant
    servant-elm servant-server stm text time transformers unliftio
    vector wai warp wreq
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base bytestring containers directory elm-bridge filepath lens
    lens-aeson mtl optparse-applicative random servant servant-elm text
    time transformers unix vector
  ];
  testHaskellDepends = [
    aeson base bytestring containers directory filepath lens lens-aeson
    mtl random text time transformers vector
  ];
  prePatch = "hpack";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
