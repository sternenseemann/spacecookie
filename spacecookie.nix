{ mkDerivation, aeson, attoparsec, base, bytestring, containers
, directory, filepath, hxt-unicode, mtl, socket, stdenv
, transformers, unix
}:
mkDerivation {
  pname = "spacecookie";
  version = "0.2.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base bytestring containers directory filepath
    hxt-unicode mtl socket transformers unix
  ];
  executableHaskellDepends = [
    aeson attoparsec base bytestring containers directory filepath mtl
    socket transformers unix
  ];
  homepage = "https://github.com/sternenseemann/spacecookie";
  description = "gopher server daemon";
  license = stdenv.lib.licenses.gpl3;
}
