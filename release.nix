let
  pkgs = import <nixpkgs> { };
in
  { books = pkgs.haskellPackages.callPackage ./default.nix { };
  }
