{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, containers, directory
      , filepath, mtl, network, stdenv, unix
      }:
      mkDerivation {
        pname = "spacecookie";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        buildDepends = [
          base bytestring containers directory filepath mtl network unix
        ];
        description = "gopher server daemon";
        license = stdenv.lib.licenses.gpl3;
      };

  drv = pkgs.haskell.packages.${compiler}.callPackage f {};

in
  drv
