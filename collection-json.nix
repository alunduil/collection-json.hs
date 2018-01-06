let
  config = {
    packageOverrides = pkgs: rec {
        haskellPackages = pkgs.haskellPackages.override {
          overrides = haskellPackagesNew: haskellPackagesOld: rec {

            collection-json  =
              haskellPackagesNew.callPackage ./default.nix { };

            network-arbitrary =
              haskellPackagesNew.callPackage ./network-arbitrary.nix { };

            network-uri-json =
              haskellPackagesNew.callPackage ./network-uri-json.nix { };

          };
        };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };
in
  { collection-json = pkgs.haskellPackages.collection-json;
  }
