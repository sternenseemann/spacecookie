let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskellPackages.override {
    extension = self: super: {
      thispackage = self.callPackage ./. {};
    };
  };
in pkgs.myEnvFun {
     name = haskellPackages.thispackage.name;
     buildInputs = [
       (haskellPackages.ghcWithPackages (hs: ([
         hs.cabalInstall
         hs.hscolour
         hs.ghcMod
       ] ++ hs.thispackage.propagatedNativeBuildInputs)))
     ];
   }      

