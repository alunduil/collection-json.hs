{ mkDerivation, aeson, base, hspec, hspec-discover
, network-arbitrary, network-uri, stdenv, test-invariant, text
}:
mkDerivation {
  pname = "network-uri-json";
  version = "0.2.0.0";
  sha256 = "39dc79718af3c39acdaeb7cea22bc06b80a38f2b2eb5396b0c70d599adbdcac6";
  libraryHaskellDepends = [ aeson base network-uri text ];
  testHaskellDepends = [
    aeson base hspec network-arbitrary network-uri test-invariant text
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/alunduil/network-uri-json";
  description = "FromJSON and ToJSON Instances for Network.URI";
  license = stdenv.lib.licenses.mit;
}
