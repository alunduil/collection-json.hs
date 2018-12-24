{ mkDerivation, base, bytestring, case-insensitive, hspec
, hspec-discover, http-media, http-types, network-uri, QuickCheck
, stdenv, test-invariant
}:
mkDerivation {
  pname = "network-arbitrary";
  version = "0.4.0.1";
  sha256 = "697e918907f3946edadd30246c041fe4c5d77f24d47eb5d165565091df303498";
  libraryHaskellDepends = [
    base bytestring http-media http-types network-uri QuickCheck
  ];
  testHaskellDepends = [
    base bytestring case-insensitive hspec http-media http-types
    network-uri QuickCheck test-invariant
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/alunduil/network-arbitrary";
  description = "Arbitrary Instances for Network Types";
  license = stdenv.lib.licenses.mit;
}
