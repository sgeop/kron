let
  config = {
    haskellPackages = pkgs.haskellPackages.override {
      overrides = haskellPackagesNew: haskellPackagesOld: rec {
        kron =
          haskellPackagesNew.callPackage ./default.nix { };
      };
    };
  };

  pkgs = import ./nixpkgs { inherit config; };

in
  { kron = pkgs.haskellPackages.callPackage ./default.nix {};
  }
