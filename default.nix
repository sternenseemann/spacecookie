let pkgs = import <nixpkgs> {};
in pkgs.haskellPackages.callPackage ./spacecookie.nix { }
