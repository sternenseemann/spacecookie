{ mkDerivation, aeson, attoparsec, base, bytestring, containers
, directory, filepath, hxt-unicode, mtl, network, stdenv
, transformers, unix
}:
mkDerivation {
  pname = "spacecookie";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base bytestring containers directory filepath
    hxt-unicode mtl network transformers unix
  ];
  executableHaskellDepends = [
    aeson attoparsec base bytestring containers directory filepath mtl
    network transformers unix
  ];
  description = "gopher server daemon";
  license = stdenv.lib.licenses.gpl3;
}
