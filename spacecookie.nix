{ mkDerivation, aeson, attoparsec, base, bytestring, containers
, directory, fast-logger, filepath, hxt-unicode, mtl, socket
, stdenv, systemd, transformers, unix
}:
mkDerivation {
  pname = "spacecookie";
  version = "0.2.1.2";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base bytestring containers directory fast-logger
    filepath hxt-unicode mtl socket transformers unix
  ];
  executableHaskellDepends = [
    aeson attoparsec base bytestring containers directory filepath mtl
    socket systemd transformers unix
  ];
  homepage = "https://github.com/sternenseemann/spacecookie";
  description = "Gopher Library and Server Daemon";
  license = stdenv.lib.licenses.gpl3;
}
