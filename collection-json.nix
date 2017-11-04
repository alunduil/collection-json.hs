{ mkDerivation, aeson, base, hspec, network-uri, QuickCheck
, quickcheck-instances, stdenv, test-invariant, text
}:
mkDerivation {
  pname = "collection-json";
  version = "1.0.1.0";
  src = ./.;
  libraryHaskellDepends = [ aeson base network-uri text ];
  testHaskellDepends = [
    aeson base hspec network-uri QuickCheck quickcheck-instances
    test-invariant text
  ];
  homepage = "https://github.com/alunduil/collection-json.hs";
  description = "Collection+JSON—Hypermedia Type Tools";
  license = stdenv.lib.licenses.mit;
}
