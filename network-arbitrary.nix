{ mkDerivation, base, bytestring, case-insensitive, fetchgit, hspec
, http-media, http-types, network-uri, QuickCheck, stdenv
, test-invariant
}:
mkDerivation {
  pname = "network-arbitrary";
  version = "0.3.0.0";
  src = fetchgit {
    url = "https://github.com/alunduil/network-arbitrary";
    sha256 = "19fdkzhmjrh24c40c266nk8wzqwb12hmi05kwb764a9xjjxy4ymm";
    rev = "e1b2fbfb0c7923b2c900d3cd14f9d5d5bb8029dc";
  };
  libraryHaskellDepends = [
    base bytestring http-media http-types network-uri QuickCheck
  ];
  testHaskellDepends = [
    base bytestring case-insensitive hspec http-media http-types
    network-uri QuickCheck test-invariant
  ];
  homepage = "https://github.com/alunduil/network-arbitrary";
  description = "Arbitrary Instances for Network Types";
  license = stdenv.lib.licenses.mit;
}
