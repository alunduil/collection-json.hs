{ mkDerivation, aeson, base, fetchgit, hspec, network-uri
, QuickCheck, stdenv, test-invariant, text
}:
mkDerivation {
  pname = "network-uri-json";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/alunduil/network-uri-json";
    sha256 = "0cj8fz7733czd83qgr0wwqagdd3z6fy0c7g46mn88vmd2rwdscfz";
    rev = "3fa5f2798a117f430a739b5d37958b48ff0450c9";
  };
  libraryHaskellDepends = [ aeson base network-uri text ];
  testHaskellDepends = [
    aeson base hspec network-uri QuickCheck test-invariant text
  ];
  homepage = "https://github.com/alunduil/network-uri-json";
  description = "FromJSON and ToJSON Instances for Network.URI";
  license = stdenv.lib.licenses.mit;
}
