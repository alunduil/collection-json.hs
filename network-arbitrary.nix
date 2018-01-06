{ mkDerivation, base, fetchgit, hspec, network-uri, QuickCheck
, stdenv
}:
mkDerivation {
  pname = "network-arbitrary";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/alunduil/network-arbitrary";
    sha256 = "0hfnzs78ljnxpyj20wwjl1q2gyifwvbw3l6bck1fhlq1zc8mb9yf";
    rev = "03944cceea89c02f97d75ef8bd0fe83a6f8962fc";
  };
  libraryHaskellDepends = [ base network-uri QuickCheck ];
  testHaskellDepends = [ base hspec network-uri QuickCheck ];
  homepage = "https://github.com/alunduil/network-arbitrary";
  description = "Arbitrary Instances for Network Types";
  license = stdenv.lib.licenses.mit;
}
