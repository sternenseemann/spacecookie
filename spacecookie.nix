{ mkDerivation, aeson, attoparsec, base, bytestring, containers
, directory, fast-logger, filepath, hxt-unicode, mtl, socket
, stdenv, systemd, tasty, tasty-hunit, text, transformers, unix
}:
mkDerivation {
  pname = "spacecookie";
  version = "0.2.1.2";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base bytestring containers directory filepath
    hxt-unicode mtl socket text transformers unix
  ];
  executableHaskellDepends = [
    aeson attoparsec base bytestring containers directory fast-logger
    filepath mtl socket systemd text transformers unix
  ];
  testHaskellDepends = [
    attoparsec base bytestring tasty tasty-hunit
  ];
  homepage = "https://github.com/sternenseemann/spacecookie";
  description = "Gopher Library and Server Daemon";
  license = stdenv.lib.licenses.gpl3;
}
