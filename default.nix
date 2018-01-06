{ mkDerivation, aeson, base, bytestring, hspec, network-arbitrary
, network-uri, network-uri-json, QuickCheck, quickcheck-instances
, stdenv, test-invariant, text
}:
mkDerivation {
  pname = "collection-json";
  version = "1.1.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base network-uri network-uri-json text
  ];
  testHaskellDepends = [
    aeson base bytestring hspec network-arbitrary network-uri
    network-uri-json QuickCheck quickcheck-instances test-invariant
    text
  ];
  homepage = "https://github.com/alunduil/collection-json.hs";
  description = "Collection+JSON—Hypermedia Type Tools";
  license = stdenv.lib.licenses.mit;
}
